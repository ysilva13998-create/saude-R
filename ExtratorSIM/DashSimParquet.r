# DASHBOARD SIM (PARQUET) - COM FILTRO DE CIDADE E MAPA
# Sistema de Informação sobre Mortalidade
# Mesmo modelo do DashSINASCparquet: lê os Parquets gerados pelo Extrator
#   <pasta_app>/Tabelas_Todas/SIM/FINAL/FINAL_SIM_<UF>_<CIDADE>_<ANO>.parquet

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi)
library(leaflet)
library(leaflet.extras)
library(purrr)

# ==============================================================================
# LOCALIZA A PASTA DO APP
# ==============================================================================
obter_pasta_app <- function() {
  # 1. Execução via Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }

  # 2. Execução via source() (ex.: Iniciar_App.bat ou Run/Source no editor)
  if (sys.nframe() > 0) {
    for (i in rev(seq_len(sys.nframe()))) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nzchar(ofile)) {
        return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
      }
    }
  }

  # 3. Fallback: getwd()
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

# ==============================================================================
# RAIZ DO APP E DA PASTA EXTRAÍDA
# ==============================================================================
pasta_app <- obter_pasta_app()
pasta_raiz <- if (tolower(basename(pasta_app)) %in% c("extratorsinasc", "extratorsim")) {
  dirname(pasta_app)
} else {
  pasta_app
}

# ==============================================================================
# CONEXÃO DUCKDB (PRINCIPAL - leitura dos Parquets)
# ==============================================================================
con <- dbConnect(duckdb::duckdb())

# ==============================================================================
# CONEXÃO DUCKDB PARA DICIONÁRIOS
# ==============================================================================
candidatos_dict <- unique(c(
  file.path(pasta_raiz, "dicionarios", "dicionarios.duckdb"),
  file.path(pasta_app, "dicionarios", "dicionarios.duckdb")
))
caminho_dict <- candidatos_dict[file.exists(candidatos_dict)][1]

if (is.na(caminho_dict) || !file.exists(caminho_dict)) {
  stop("Não foi possível encontrar 'dicionarios.duckdb'. Pasta do app: ", pasta_app)
}

caminho_dict <- normalizePath(caminho_dict, winslash = "/", mustWork = TRUE)
con_dict <- dbConnect(duckdb(), caminho_dict, read_only = TRUE)

# Municípios
municipios_df <- as.data.frame(dbGetQuery(con_dict, "
    SELECT
        id_municipio_6 as id6,
        nome,
        sigla_uf
    FROM municipios
"))
municipios_df$id6 <- as.character(municipios_df$id6)

# Lookup rápido código -> nome (vetorizado, bem mais rápido que sapply)
lookup_municipios <- setNames(municipios_df$nome, municipios_df$id6)

nome_municipio_vec <- function(cod) {
  cod <- as.character(cod)
  cod <- ifelse(is.na(cod) | cod == "", NA_character_, str_pad(substr(cod, 1, 6), 6, pad = "0"))
  nomes <- unname(lookup_municipios[cod])
  ifelse(is.na(nomes), cod, nomes)
}

# Normaliza texto (minúsculo, sem acento, sem "_") para comparar nomes de cidade
normalizar_nome <- function(x) {
  x <- stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  x <- gsub("[_\\-]+", " ", x)
  tolower(trimws(x))
}

codigo_municipio <- function(nome_cidade) {
  if (is.null(nome_cidade) || is.na(nome_cidade) || nome_cidade %in% c("", "Todas")) return(NA_character_)
  idx <- which(normalizar_nome(municipios_df$nome) == normalizar_nome(nome_cidade))
  if (length(idx) == 0) return(NA_character_)
  municipios_df$id6[idx[1]]
}

# Dicionário CID-10 (causas de morte)
cid_dict <- tryCatch({
  d <- as.data.frame(dbGetQuery(con_dict, "SELECT subcat, descricao FROM cid10_subcategorias"))
  d$subcat <- as.character(d$subcat)
  distinct(d, subcat, .keep_all = TRUE)
}, error = function(e) {
  message("Dicionário CID não carregado: ", conditionMessage(e))
  data.frame(subcat = character(0), descricao = character(0), stringsAsFactors = FALSE)
})

# ==============================================================================
# LOCALIZAÇÃO E CARGA DO CNES (<pasta_raiz>/dicionarios/cnes.duckdb)
# ==============================================================================
candidatos_cnes <- unique(c(
  file.path(pasta_raiz, "dicionarios", "cnes.duckdb"),
  file.path(pasta_app, "dicionarios", "cnes.duckdb")
))
cnes_cand <- candidatos_cnes[file.exists(candidatos_cnes)][1]
cnes_path <- if (!is.na(cnes_cand) && file.exists(cnes_cand)) {
  normalizePath(cnes_cand, winslash = "/", mustWork = TRUE)
} else {
  NULL
}

# Carregado UMA vez na inicialização (não a cada troca de filtro)
cnes_completo <- NULL
if (!is.null(cnes_path)) {
  cnes_completo <- tryCatch({
    con_cnes <- dbConnect(duckdb(), cnes_path, read_only = TRUE)
    on.exit(dbDisconnect(con_cnes, shutdown = TRUE), add = TRUE)
    d <- as.data.frame(dbGetQuery(con_cnes,
      "SELECT cnes, ibge, latitude, longitude, nome_estabelecimento FROM cnes"))
    d$cnes <- str_pad(as.character(d$cnes), 7, pad = "0")
    d$latitude  <- suppressWarnings(as.numeric(d$latitude))
    d$longitude <- suppressWarnings(as.numeric(d$longitude))
    distinct(d, cnes, .keep_all = TRUE)
  }, error = function(e) {
    message("CNES não carregado: ", conditionMessage(e))
    NULL
  })
} else {
  message("cnes.duckdb não encontrado em: ", paste(candidatos_cnes, collapse = " | "))
}

# ==============================================================================
# CONFIGURAÇÃO DOS PARQUETS (<pasta_app>/Tabelas_Todas/SIM/FINAL)
# ==============================================================================
PATH_DADOS <- file.path(pasta_app, "Tabelas_Todas")

listar_parquets_sim <- function() {
  # Aceita variações de nome de pasta usadas pelo extrator
  pastas <- file.path(PATH_DADOS, c("SIM", "SIM-DO", "SIM_DO", "SIMDO"), "FINAL")
  pastas <- pastas[dir.exists(pastas)]
  if (length(pastas) == 0) return(character(0))

  arquivos <- unlist(lapply(pastas, function(p) {
    list.files(p, pattern = "^FINAL_SIM.*\\.parquet$", full.names = TRUE, ignore.case = TRUE)
  }))
  if (length(arquivos) == 0) return(character(0))
  normalizePath(arquivos, winslash = "/", mustWork = FALSE)
}

# Formato: FINAL_SIM_UF_CIDADE_ANO.parquet  (ex.: FINAL_SIM_PR_Irati_2024.parquet)
# Cidades com "_" no nome (ex.: Ponta_Grossa) também são tratadas.
extrair_metadados_parquet <- function(caminho_arquivo) {
  nome <- basename(caminho_arquivo)
  nome_sem_ext <- gsub("\\.parquet$", "", nome, ignore.case = TRUE)
  partes <- strsplit(nome_sem_ext, "_")[[1]]

  resultado <- list(
    caminho = caminho_arquivo,
    uf = NA_character_,
    cidade = NA_character_,
    ano = NA_integer_
  )

  n <- length(partes)
  if (n >= 5) {
    resultado$uf <- partes[3]
    resultado$cidade <- paste(partes[4:(n - 1)], collapse = " ")

    ano_valor <- suppressWarnings(as.numeric(partes[n]))
    if (!is.na(ano_valor) && ano_valor > 1900 && ano_valor < 2100) {
      resultado$ano <- ano_valor
    }
  }

  resultado
}

metadados_parquets <- function() {
  arquivos <- listar_parquets_sim()
  if (length(arquivos) == 0) {
    return(data.frame(caminho = character(0), uf = character(0),
                      cidade = character(0), ano = numeric(0),
                      stringsAsFactors = FALSE))
  }
  md <- purrr::map_df(arquivos, function(x) {
    m <- extrair_metadados_parquet(x)
    data.frame(caminho = m$caminho, uf = m$uf, cidade = m$cidade, ano = m$ano,
               stringsAsFactors = FALSE)
  })
  md %>% filter(!is.na(ano), !is.na(cidade), cidade != "")
}

# ==============================================================================
# CIDADES E ANOS DISPONÍVEIS
# ==============================================================================
obter_cidades_disponiveis <- function() {
  md <- metadados_parquets()
  message("Parquets SIM encontrados: ", nrow(md))
  if (nrow(md) == 0) return(character(0))
  sort(unique(md$cidade))
}

obter_anos_disponiveis <- function(cidade_selecionada = NULL) {
  md <- metadados_parquets()
  if (nrow(md) == 0) return(integer(0))

  if (!is.null(cidade_selecionada) && cidade_selecionada != "Todas" && cidade_selecionada != "") {
    md <- md %>% filter(tolower(cidade) == tolower(cidade_selecionada))
  }
  sort(unique(md$ano), decreasing = TRUE)
}

# ==============================================================================
# CONVERSÃO DA IDADE DO SIM
# ==============================================================================
converter_idade_sim <- function(idade_raw) {
  idade_raw <- suppressWarnings(as.numeric(as.character(idade_raw)))
  case_when(
    is.na(idade_raw) ~ NA_real_,
    idade_raw == 999 ~ NA_real_,                      # idade ignorada
    idade_raw < 400 ~ 0,                              # minutos/horas/dias/meses -> < 1 ano
    idade_raw >= 400 & idade_raw < 500 ~ idade_raw - 400,
    idade_raw >= 500 ~ idade_raw - 500 + 100,
    TRUE ~ NA_real_
  )
}

# ==============================================================================
# FUNÇÃO PARA CARREGAR DADOS SIM COM DUCKDB (PARQUET)
# ==============================================================================
carregar_sim_parquet <- function(cidade_filtro = NULL, ano_filtro = NULL, mes_filtro = NULL) {

  metadados <- metadados_parquets()

  if (nrow(metadados) == 0) {
    showNotification("Nenhum arquivo Parquet encontrado! Extraia dados primeiro.", type = "error")
    return(NULL)
  }

  # Aplicar filtros
  if (!is.null(cidade_filtro) && cidade_filtro != "Todas" && cidade_filtro != "") {
    metadados <- metadados %>% filter(tolower(cidade) == tolower(cidade_filtro))
  }
  if (!is.null(ano_filtro)) {
    metadados <- metadados %>% filter(ano == ano_filtro)
  }

  if (nrow(metadados) == 0) {
    showNotification("Nenhum arquivo encontrado para os filtros selecionados.", type = "warning")
    return(NULL)
  }

  # ==============================================
  # LEITURA COM DUCKDB - UNIÃO DE TODOS OS PARQUETS
  # ==============================================
  caminhos_sql <- paste0("'", gsub("'", "''", metadados$caminho), "'", collapse = ", ")

  query <- sprintf(
    "SELECT * FROM read_parquet([%s], union_by_name = true, filename = true)",
    caminhos_sql
  )

  df <- dbGetQuery(con, query)

  if (nrow(df) == 0) return(NULL)

  names(df) <- toupper(names(df))

  # Cidade/UF de origem (por arquivo)
  md_map <- metadados %>%
    mutate(caminho_n = normalizePath(caminho, winslash = "/", mustWork = FALSE)) %>%
    select(caminho_n, CIDADE_ORIGEM = cidade, UF_ORIGEM = uf)
  df$FILENAME <- normalizePath(df$FILENAME, winslash = "/", mustWork = FALSE)
  df <- df %>%
    left_join(md_map, by = c("FILENAME" = "caminho_n")) %>%
    select(-FILENAME)

  # Helper: pega a primeira coluna existente
  pega_col <- function(opcoes) {
    achou <- intersect(names(df), opcoes)[1]
    if (is.na(achou)) rep(NA, nrow(df)) else df[[achou]]
  }

  # ---------------- Data / mês do óbito ----------------
  dt <- pega_col(c("DTOBITO", "DATA_OBITO"))
  if (inherits(dt, c("Date", "POSIXct"))) {
    df$DTOBITO <- format(dt, "%d%m%Y")
    mes <- as.numeric(format(dt, "%m"))
  } else {
    dt <- as.character(dt)
    df$DTOBITO <- dt
    mes <- ifelse(
      grepl("^\\d{4}-\\d{2}", dt),
      suppressWarnings(as.numeric(substr(dt, 6, 7))),               # AAAA-MM-DD
      suppressWarnings(as.numeric(substr(str_pad(dt, 8, pad = "0"), 3, 4)))  # DDMMAAAA
    )
  }
  df$MES <- ifelse(is.na(mes) | mes < 1 | mes > 12, 1, mes)
  df$MES_NOME <- factor(df$MES, levels = 1:12,
                        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                   "Jul", "Ago", "Set", "Out", "Nov", "Dez"))

  # ---------------- Idade ----------------
  df$IDADE_RAW  <- suppressWarnings(as.numeric(as.character(pega_col(c("IDADE")))))
  df$IDADE_ANOS <- converter_idade_sim(df$IDADE_RAW)

  # ---------------- Sexo ----------------
  sexo <- as.character(pega_col(c("SEXO")))
  df$SEXO_LABEL <- case_when(
    sexo == "1" ~ "Masculino",
    sexo == "2" ~ "Feminino",
    TRUE ~ "Ignorado"
  )

  # ---------------- Raça/Cor ----------------
  raca <- as.character(pega_col(c("RACACOR")))
  df$RACA_LABEL <- case_when(
    raca == "1" ~ "Branca",
    raca == "2" ~ "Preta",
    raca == "3" ~ "Amarela",
    raca == "4" ~ "Parda",
    raca == "5" ~ "Indígena",
    TRUE ~ "Ignorado"
  )

  # ---------------- Estado civil ----------------
  ec <- as.character(pega_col(c("ESTCIV")))
  df$ESTCIV_LABEL <- case_when(
    ec == "1" ~ "Solteiro",
    ec == "2" ~ "Casado",
    ec == "3" ~ "Viúvo",
    ec == "4" ~ "Separado/Divorciado",
    ec == "5" ~ "União Estável",
    TRUE ~ "Ignorado"
  )

  # ---------------- Escolaridade ----------------
  esc <- as.character(pega_col(c("ESC")))
  df$ESCOLARIDADE <- case_when(
    esc == "1" ~ "Nenhuma",
    esc == "2" ~ "1º grau incompleto",
    esc == "3" ~ "1º grau completo",
    esc == "4" ~ "2º grau incompleto",
    esc == "5" ~ "2º grau completo",
    esc == "6" ~ "Superior incompleto",
    esc == "7" ~ "Superior completo",
    TRUE ~ "Ignorado"
  )

  # ---------------- Causa básica (CID) ----------------
  causa <- as.character(pega_col(c("CAUSABAS", "CAUSA_BASICA")))
  df$CAUSA_COD <- ifelse(is.na(causa) | causa == "", "Ignorado", substr(causa, 1, 4))

  df <- df %>%
    left_join(cid_dict, by = c("CAUSA_COD" = "subcat")) %>%
    mutate(CAUSA_DESCRICAO = ifelse(is.na(descricao), CAUSA_COD, descricao)) %>%
    select(-descricao)

  # ---------------- Estabelecimento (CNES) ----------------
  cnes_raw <- as.character(pega_col(c("CODESTAB", "ESTAB", "CNES")))
  df$CNES <- ifelse(is.na(cnes_raw) | cnes_raw == "", NA_character_,
                    str_pad(cnes_raw, 7, pad = "0"))

  if (!is.null(cnes_completo)) {
    df <- df %>%
      left_join(select(cnes_completo, cnes, latitude, longitude, nome_estabelecimento),
                by = c("CNES" = "cnes")) %>%
      mutate(CNES_NOME = ifelse(is.na(nome_estabelecimento), CNES, nome_estabelecimento)) %>%
      select(-nome_estabelecimento)
  } else {
    df$CNES_NOME <- df$CNES
    df$latitude  <- NA_real_
    df$longitude <- NA_real_
  }

  # ---------------- Municípios ----------------
  df$CODMUNRES  <- substr(as.character(pega_col(c("CODMUNRES"))), 1, 6)
  df$CODMUNOCOR <- substr(as.character(pega_col(c("CODMUNOCOR"))), 1, 6)
  df$CIDADE_RESIDENCIA  <- nome_municipio_vec(df$CODMUNRES)
  df$CIDADE_OCORRENCIA  <- nome_municipio_vec(df$CODMUNOCOR)

  # ---------------- Faixa etária ----------------
  df$FAIXA_IDADE <- case_when(
    is.na(df$IDADE_ANOS) ~ "Ignorado",
    df$IDADE_ANOS < 1 ~ "< 1 ano",
    df$IDADE_ANOS < 15 ~ "1-14 anos",
    df$IDADE_ANOS < 30 ~ "15-29 anos",
    df$IDADE_ANOS < 45 ~ "30-44 anos",
    df$IDADE_ANOS < 60 ~ "45-59 anos",
    df$IDADE_ANOS < 75 ~ "60-74 anos",
    TRUE ~ "75+ anos"
  )

  # ---------------- Tipo de fluxo (cidade selecionada) ----------------
  codigo_ref <- codigo_municipio(cidade_filtro)
  if (!is.na(codigo_ref)) {
    cidade_ref <- cidade_filtro
    df$TIPO_FLUXO <- case_when(
      df$CODMUNRES == codigo_ref & df$CODMUNOCOR == codigo_ref ~ paste("Reside e Morre em", cidade_ref),
      df$CODMUNRES == codigo_ref & df$CODMUNOCOR != codigo_ref ~ paste("Apenas Reside em", cidade_ref),
      df$CODMUNRES != codigo_ref & df$CODMUNOCOR == codigo_ref ~ paste("Apenas Morre em", cidade_ref),
      TRUE ~ "Outros"
    )
  } else {
    df$TIPO_FLUXO <- "Todas as cidades"
  }

  # ---------------- Filtro por mês ----------------
  if (!is.null(mes_filtro) && mes_filtro != "Todos") {
    df <- df %>% filter(MES == as.numeric(mes_filtro))
  }

  df
}

options(scipen = 999)

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = uiOutput("titulo_dashboard")),

  dashboardSidebar(
    sidebarMenu(

      # ========== FILTROS ==========
      selectInput("cidade_sim", tags$b(icon("city"), " Cidade:"),
                  choices = NULL, selected = NULL),

      selectInput("ano_sim", "Selecione o Ano:", choices = NULL),

      selectInput("mes_sim", "Selecione o Mês:",
                  choices = c("Todos", "Janeiro" = 1, "Fevereiro" = 2, "Março" = 3, "Abril" = 4,
                              "Maio" = 5, "Junho" = 6, "Julho" = 7, "Agosto" = 8,
                              "Setembro" = 9, "Outubro" = 10, "Novembro" = 11, "Dezembro" = 12),
                  selected = "Todos"),

      numericInput("pop_ref", "População de referência:", value = 60000, min = 1, step = 1000),
      hr(),
      menuItem("Visão Geral", tabName = "geral", icon = icon("tachometer-alt")),
      menuItem("Óbitos", tabName = "obitos", icon = icon("skull")),
      menuItem("Causas de Morte", tabName = "causas", icon = icon("heartbeat")),
      menuItem("Perfil Demográfico", tabName = "demografico", icon = icon("users")),
      menuItem("Análise de Idosos (60+)", tabName = "idosos", icon = icon("user-plus")),
      menuItem("Estabelecimentos", tabName = "estabelecimentos", icon = icon("hospital")),
      menuItem("🗺️ Mapa dos Óbitos", tabName = "mapa", icon = icon("map-marker-alt")),
      menuItem("Dados Brutos", tabName = "dados", icon = icon("table")),
      menuItem("Relatório", tabName = "relatorio", icon = icon("file-alt"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .info-box { border-radius: 10px; }
        .small-box { border-radius: 10px; }
        .leaflet-container { border-radius: 10px; }
      "))
    ),

    tabItems(
      # ========== VISÃO GERAL ==========
      tabItem(
        tabName = "geral",
        fluidRow(
          valueBoxOutput("total_obitos", width = 3),
          valueBoxOutput("taxa_mortalidade", width = 3),
          valueBoxOutput("mortalidade_infantil", width = 3),
          valueBoxOutput("media_idade", width = 3)
        ),
        fluidRow(
          valueBoxOutput("percentual_masculino", width = 3),
          valueBoxOutput("percentual_feminino", width = 3),
          valueBoxOutput("obitos_acima_60", width = 3),
          valueBoxOutput("obitos_menos_1", width = 3)
        ),
        fluidRow(
          box(title = "Evolução Mensal dos Óbitos", width = 12, status = "danger",
              plotlyOutput("evolucao_mensal", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Distribuição por Faixa Etária", width = 6, status = "danger",
              plotlyOutput("faixa_etaria", height = "400px")
          ),
          box(title = "Distribuição por Sexo", width = 6, status = "danger",
              plotlyOutput("sexo_pizza", height = "400px")
          )
        )
      ),

      # ========== ÓBITOS ==========
      tabItem(
        tabName = "obitos",
        fluidRow(
          box(title = "Óbitos por Mês", width = 12, status = "warning",
              plotlyOutput("obitos_mensal", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Distribuição dos Óbitos por Idade", width = 12, status = "info",
              plotlyOutput("obitos_idade", height = "400px")
          )
        )
      ),

      # ========== CAUSAS DE MORTE ==========
      tabItem(
        tabName = "causas",
        fluidRow(
          box(title = "Principais Causas de Morte (com tradução CID)", width = 12, status = "danger",
              plotlyOutput("causas_top", height = "600px")
          )
        ),
        fluidRow(
          box(title = "Buscar CID Específico", width = 12, status = "warning",
              textInput("buscar_cid", "Digite o código CID (ex: I10, E11, J44):", value = ""),
              br(),
              DTOutput("tabela_cid_busca")
          )
        )
      ),

      # ========== PERFIL DEMOGRÁFICO ==========
      tabItem(
        tabName = "demografico",
        fluidRow(
          box(title = "Óbitos por Raça/Cor", width = 6, status = "primary",
              plotlyOutput("raca_barras", height = "400px")
          ),
          box(title = "Óbitos por Estado Civil", width = 6, status = "primary",
              plotlyOutput("estado_civil", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Óbitos por Escolaridade", width = 12, status = "info",
              plotlyOutput("escolaridade_barras", height = "400px")
          )
        )
      ),

      # ========== ANÁLISE DE IDOSOS (60+) ==========
      tabItem(
        tabName = "idosos",
        fluidRow(
          box(title = "Resumo de Óbitos em Idosos (60+ anos)", width = 12, status = "danger",
              infoBoxOutput("idosos_total", width = 3),
              infoBoxOutput("idosos_residentes", width = 3),
              infoBoxOutput("idosos_ocorridos", width = 3),
              infoBoxOutput("idosos_ambos", width = 3)
          )
        ),
        fluidRow(
          box(title = "Principais Causas de Morte em Idosos (60+)", width = 12, status = "warning",
              plotlyOutput("causas_top_idosos", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Distribuição Etária dos Idosos (60+)", width = 6, status = "info",
              plotlyOutput("idade_idosos", height = "400px")
          ),
          box(title = "📊 Fluxo de Óbitos em Idosos", width = 6, status = "danger",
              plotlyOutput("fluxo_idosos", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Cidades de Ocorrência dos Óbitos de Idosos Residentes", width = 12, status = "warning",
              plotlyOutput("destinos_idosos", height = "500px")
          )
        )
      ),

      # ========== ESTABELECIMENTOS ==========
      tabItem(
        tabName = "estabelecimentos",
        fluidRow(
          box(title = "Óbitos por Estabelecimento", width = 12, status = "warning",
              plotlyOutput("obitos_estabelecimento", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Comparativo Mensal - Top 5", width = 12, status = "warning",
              plotlyOutput("comparativo_estabelecimentos", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Buscar Estabelecimento", width = 12, status = "info",
              textInput("buscar_estabelecimento", "Digite o nome do estabelecimento:",
                        placeholder = "Ex: Santa Casa, Hospital"),
              br(),
              DTOutput("tabela_estab_busca")
          )
        )
      ),

      # ========== MAPA DOS ÓBITOS ==========
      tabItem(
        tabName = "mapa",
        fluidRow(
          box(
            title = "🗺️ Localização dos Óbitos por Estabelecimento",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            div(
              style = "margin-bottom: 15px;",
              radioButtons("mapa_tipo_sim", "Tipo de Visualização:",
                           choices = c(
                             "📍 Pontos" = "pontos",
                             "🔥 Heatmap" = "heatmap",
                             "🔵 Cluster" = "cluster"
                           ),
                           inline = TRUE
              )),
            leafletOutput("mapa_obitos", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "📊 Resumo dos Estabelecimentos no Mapa",
            width = 12,
            status = "info",
            DTOutput("tabela_estabelecimentos_mapa")
          )
        )
      ),

      # ========== DADOS BRUTOS ==========
      tabItem(
        tabName = "dados",
        fluidRow(
          box(title = "Dados SIM com Tradução CID e Nomes CNES", width = 12, status = "primary",
              DTOutput("tabela_sim"),
              br(),
              downloadButton("download_sim", "📥 Baixar Dados (CSV)", class = "btn-success")
          )
        )
      ),

      # ========== RELATÓRIO ==========
      tabItem(
        tabName = "relatorio",
        fluidRow(
          box(title = "Relatório de Mortalidade", width = 12, status = "danger",
              downloadButton("download_relatorio_sim", "📥 Baixar Relatório (HTML)", class = "btn-success"),
              br(), br(),
              htmlOutput("relatorio_sim")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ========== POPULAÇÃO (referência informada pelo usuário) ==========
  populacao <- reactive({
    p <- input$pop_ref
    if (is.null(p) || is.na(p) || p <= 0) 60000 else p
  })

  # ========== CARREGAR CIDADES DISPONÍVEIS ==========
  observe({
    cidades <- obter_cidades_disponiveis()

    if (length(cidades) > 0) {
      opcoes <- c("Todas", cidades)
      updateSelectInput(session, "cidade_sim", choices = opcoes, selected = "Todas")
    } else {
      updateSelectInput(session, "cidade_sim", choices = c("Nenhum dado encontrado" = ""), selected = "")
      showNotification("Nenhum arquivo Parquet do SIM encontrado! Use o Extrator DATASUS primeiro.",
                       type = "error", duration = 10)
    }
  })

  # ========== CARREGAR ANOS DISPONÍVEIS (dinâmico conforme cidade) ==========
  observeEvent(input$cidade_sim, {
    req(input$cidade_sim != "", input$cidade_sim != "Nenhum dado encontrado")

    cidade_param <- if (input$cidade_sim == "Todas") NULL else input$cidade_sim
    anos <- obter_anos_disponiveis(cidade_param)

    if (length(anos) > 0) {
      updateSelectInput(session, "ano_sim", choices = anos, selected = anos[1])
    } else {
      updateSelectInput(session, "ano_sim", choices = c("Nenhum ano encontrado" = ""), selected = "")
    }
  })

  # ========== TÍTULO DINÂMICO ==========
  output$titulo_dashboard <- renderUI({
    if (is.null(input$cidade_sim) || input$cidade_sim == "" || input$cidade_sim == "Todas") {
      paste("SIM - Sistema de Informação sobre Mortalidade - Todas as Cidades")
    } else {
      paste("SIM - Sistema de Informação sobre Mortalidade -", input$cidade_sim)
    }
  })

  # ========== CIDADE DE REFERÊNCIA ==========
  cidade_ref <- reactive({
    if (is.null(input$cidade_sim) || input$cidade_sim %in% c("", "Todas")) NULL else input$cidade_sim
  })

  codigo_ref <- reactive({
    codigo_municipio(cidade_ref())
  })

  # ========== CARREGAR DADOS SIM ==========
  sim_data <- reactive({
    req(input$ano_sim, input$cidade_sim, input$cidade_sim != "", input$cidade_sim != "Nenhum dado encontrado")
    req(input$ano_sim != "Nenhum ano encontrado")

    cidade_param <- if (input$cidade_sim == "Todas") NULL else input$cidade_sim

    df <- carregar_sim_parquet(
      cidade_filtro = cidade_param,
      ano_filtro = as.numeric(input$ano_sim),
      mes_filtro = input$mes_sim
    )

    if (is.null(df) || nrow(df) == 0) {
      showNotification("Nenhum dado encontrado para os filtros selecionados.", type = "warning")
      return(NULL)
    }

    df
  })

  # Dados apenas de idosos (60+)
  sim_idosos <- reactive({
    req(sim_data())
    sim_data() %>% filter(IDADE_ANOS >= 60)
  })

  # ========== DADOS PARA MAPA ==========
  dados_mapa <- reactive({
    req(sim_data())

    df <- sim_data() %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      group_by(CNES, CNES_NOME, latitude, longitude) %>%
      summarise(
        total_obitos = n(),
        idade_media = round(mean(IDADE_ANOS, na.rm = TRUE), 1),
        perc_masculino = round(sum(SEXO_LABEL == "Masculino", na.rm = TRUE) / n() * 100, 1),
        perc_feminino = round(sum(SEXO_LABEL == "Feminino", na.rm = TRUE) / n() * 100, 1),
        obitos_60_mais = sum(IDADE_ANOS >= 60, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        popup_text = paste0(
          "<b>", CNES_NOME, "</b><br>",
          "💀 Total: ", total_obitos, "<br>",
          "📊 Idade média: ", idade_media, " anos<br>",
          "👨 Homens: ", perc_masculino, "%<br>",
          "👩 Mulheres: ", perc_feminino, "%<br>",
          "👴 60+: ", obitos_60_mais, " (", round(obitos_60_mais / total_obitos * 100, 1), "%)"
        )
      )

    df
  })

  # ========== VISÃO GERAL ==========

  output$total_obitos <- renderValueBox({
    req(sim_data())
    cidade_nome <- if (is.null(cidade_ref())) "Todas as Cidades" else cidade_ref()
    valueBox(
      value = format(nrow(sim_data()), big.mark = "."),
      subtitle = paste("Total de Óbitos -", cidade_nome),
      icon = icon("skull"),
      color = "red"
    )
  })

  output$taxa_mortalidade <- renderValueBox({
    req(sim_data(), populacao())
    taxa <- round(nrow(sim_data()) / populacao() * 1000, 2)
    valueBox(
      value = paste(taxa, "/1.000 hab"),
      subtitle = "Taxa de Mortalidade (pop. de referência)",
      icon = icon("chart-line"),
      color = "blue"
    )
  })

  output$mortalidade_infantil <- renderValueBox({
    req(sim_data())
    obitos_inf <- sum(sim_data()$IDADE_ANOS < 1, na.rm = TRUE)
    perc <- ifelse(nrow(sim_data()) > 0, round(obitos_inf / nrow(sim_data()) * 100, 2), 0)
    valueBox(
      value = paste0(obitos_inf, " (", perc, "%)"),
      subtitle = "Óbitos Infantis (< 1 ano)",
      icon = icon("baby"),
      color = "yellow"
    )
  })

  output$media_idade <- renderValueBox({
    req(sim_data())
    media <- round(mean(sim_data()$IDADE_ANOS, na.rm = TRUE), 1)
    valueBox(
      value = paste(media, "anos"),
      subtitle = "Idade Média",
      icon = icon("calendar"),
      color = "teal"
    )
  })

  output$percentual_masculino <- renderValueBox({
    req(sim_data())
    perc <- round(sum(sim_data()$SEXO_LABEL == "Masculino", na.rm = TRUE) / nrow(sim_data()) * 100, 1)
    valueBox(
      value = paste(perc, "%"),
      subtitle = "Sexo Masculino",
      icon = icon("mars"),
      color = "light-blue"
    )
  })

  output$percentual_feminino <- renderValueBox({
    req(sim_data())
    perc <- round(sum(sim_data()$SEXO_LABEL == "Feminino", na.rm = TRUE) / nrow(sim_data()) * 100, 1)
    valueBox(
      value = paste(perc, "%"),
      subtitle = "Sexo Feminino",
      icon = icon("venus"),
      color = "fuchsia"
    )
  })

  output$obitos_acima_60 <- renderValueBox({
    req(sim_data())
    obitos <- sum(sim_data()$IDADE_ANOS >= 60, na.rm = TRUE)
    perc <- round(obitos / nrow(sim_data()) * 100, 1)
    valueBox(
      value = paste0(obitos, " (", perc, "%)"),
      subtitle = "Óbitos acima de 60 anos",
      icon = icon("user-plus"),
      color = "orange"
    )
  })

  output$obitos_menos_1 <- renderValueBox({
    req(sim_data())
    obitos <- sum(sim_data()$IDADE_ANOS < 1, na.rm = TRUE)
    valueBox(
      value = obitos,
      subtitle = "Óbitos com menos de 1 ano",
      icon = icon("baby"),
      color = "purple"
    )
  })

  # ========== GRÁFICOS ==========

  output$evolucao_mensal <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      group_by(MES, MES_NOME) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, type = "scatter", mode = "lines+markers",
            line = list(color = "#e74c3c", width = 2),
            marker = list(color = "#c0392b", size = 8)) %>%
      layout(title = paste("Óbitos por Mês -", input$ano_sim),
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Número de Óbitos"))
  })

  output$faixa_etaria <- renderPlotly({
    req(sim_data())
    ordem <- c("< 1 ano", "1-14 anos", "15-29 anos", "30-44 anos", "45-59 anos", "60-74 anos", "75+ anos", "Ignorado")
    df <- sim_data() %>%
      group_by(FAIXA_IDADE) %>%
      summarise(Total = n(), .groups = 'drop')
    df$FAIXA_IDADE <- factor(df$FAIXA_IDADE, levels = ordem)
    df <- df %>% arrange(FAIXA_IDADE)

    plot_ly(df, x = ~FAIXA_IDADE, y = ~Total, type = "bar",
            marker = list(color = "#e74c3c")) %>%
      layout(title = "Óbitos por Faixa Etária",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Óbitos"))
  })

  output$sexo_pizza <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      group_by(SEXO_LABEL) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, labels = ~SEXO_LABEL, values = ~Total, type = "pie",
            marker = list(colors = c("#e74c3c", "#3498db", "#95a5a6"))) %>%
      layout(title = "Distribuição por Sexo")
  })

  # ========== ÓBITOS ==========

  output$obitos_mensal <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      group_by(MES, MES_NOME) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, type = "bar",
            marker = list(color = "#f39c12")) %>%
      layout(title = paste("Óbitos por Mês -", input$ano_sim),
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Óbitos"))
  })

  output$obitos_idade <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>% filter(!is.na(IDADE_ANOS) & IDADE_ANOS <= 120)

    plot_ly(df, x = ~IDADE_ANOS, type = "histogram", nbinsx = 30,
            marker = list(color = "#3498db")) %>%
      layout(title = "Distribuição dos Óbitos por Idade",
             xaxis = list(title = "Idade (anos)"),
             yaxis = list(title = "Frequência"))
  })

  # ========== CAUSAS DE MORTE ==========

  output$causas_top <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      filter(CAUSA_COD != "Ignorado") %>%
      group_by(CAUSA_COD, CAUSA_DESCRICAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)

    plot_ly(df, x = ~Total, y = ~reorder(CAUSA_DESCRICAO, Total), type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = paste("Principais Causas de Morte -", input$ano_sim),
             xaxis = list(title = "Número de Óbitos"),
             yaxis = list(title = ""))
  })

  output$tabela_cid_busca <- renderDT({
    req(sim_data())

    if (!is.null(input$buscar_cid) && input$buscar_cid != "") {
      busca <- toupper(input$buscar_cid)
      df <- sim_data() %>%
        filter(grepl(busca, CAUSA_COD, ignore.case = TRUE) |
                 grepl(busca, CAUSA_DESCRICAO, ignore.case = TRUE)) %>%
        group_by(CAUSA_COD, CAUSA_DESCRICAO) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))

      datatable(df, options = list(pageLength = 10), rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_cid))
    } else {
      datatable(data.frame(Mensagem = "Digite um código CID para buscar"), rownames = FALSE)
    }
  })

  # ========== PERFIL DEMOGRÁFICO ==========

  output$raca_barras <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      filter(RACA_LABEL != "Ignorado") %>%
      group_by(RACA_LABEL) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~RACA_LABEL, y = ~Total, type = "bar",
            marker = list(color = "#9b59b6")) %>%
      layout(title = "Óbitos por Raça/Cor",
             xaxis = list(title = ""),
             yaxis = list(title = "Óbitos"))
  })

  output$estado_civil <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      filter(ESTCIV_LABEL != "Ignorado") %>%
      group_by(ESTCIV_LABEL) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~ESTCIV_LABEL, y = ~Total, type = "bar",
            marker = list(color = "#1abc9c")) %>%
      layout(title = "Óbitos por Estado Civil",
             xaxis = list(title = ""),
             yaxis = list(title = "Óbitos"))
  })

  output$escolaridade_barras <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      filter(ESCOLARIDADE != "Ignorado") %>%
      group_by(ESCOLARIDADE) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~ESCOLARIDADE, y = ~Total, type = "bar",
            marker = list(color = "#3498db")) %>%
      layout(title = "Óbitos por Escolaridade",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Óbitos"))
  })

  # ========== ANÁLISE DE IDOSOS ==========

  output$idosos_total <- renderInfoBox({
    req(sim_idosos())
    infoBox(
      title = "Total de Óbitos (60+)",
      value = nrow(sim_idosos()),
      icon = icon("user-plus"),
      color = "red",
      fill = TRUE
    )
  })

  output$idosos_residentes <- renderInfoBox({
    req(sim_idosos())
    cod <- codigo_ref()
    valor <- if (is.na(cod)) "—" else sum(sim_idosos()$CODMUNRES == cod, na.rm = TRUE)
    infoBox(
      title = if (is.null(cidade_ref())) "Residentes (selecione uma cidade)" else paste("Residentes em", cidade_ref()),
      value = valor,
      icon = icon("home"),
      color = "blue",
      fill = TRUE
    )
  })

  output$idosos_ocorridos <- renderInfoBox({
    req(sim_idosos())
    cod <- codigo_ref()
    valor <- if (is.na(cod)) "—" else sum(sim_idosos()$CODMUNOCOR == cod, na.rm = TRUE)
    infoBox(
      title = if (is.null(cidade_ref())) "Ocorridos (selecione uma cidade)" else paste("Ocorridos em", cidade_ref()),
      value = valor,
      icon = icon("hospital"),
      color = "green",
      fill = TRUE
    )
  })

  output$idosos_ambos <- renderInfoBox({
    req(sim_idosos())
    cod <- codigo_ref()
    valor <- if (is.na(cod)) "—" else sum(sim_idosos()$CODMUNRES == cod & sim_idosos()$CODMUNOCOR == cod, na.rm = TRUE)
    infoBox(
      title = "Ambos (Reside e Ocorre)",
      value = valor,
      icon = icon("check-circle"),
      color = "yellow",
      fill = TRUE
    )
  })

  # ========== FLUXO DE IDOSOS ==========
  output$fluxo_idosos <- renderPlotly({
    req(sim_idosos())
    req(!is.na(codigo_ref()))

    cod <- codigo_ref()
    cidade <- cidade_ref()

    df <- sim_idosos() %>% filter(CODMUNRES == cod)

    total_idosos <- nrow(df)
    morrem_aqui <- sum(df$CODMUNOCOR == cod, na.rm = TRUE)
    morrem_fora <- total_idosos - morrem_aqui
    perc_aqui <- if (total_idosos > 0) round(morrem_aqui / total_idosos * 100, 1) else 0
    perc_fora <- if (total_idosos > 0) round(morrem_fora / total_idosos * 100, 1) else 0

    dados_fluxo <- data.frame(
      Categoria = c(paste("✅ Morrem em", cidade), paste("❌ Morrem fora de", cidade)),
      Quantidade = c(morrem_aqui, morrem_fora),
      Percentual = c(perc_aqui, perc_fora)
    )

    plot_ly(dados_fluxo,
            x = ~Categoria,
            y = ~Quantidade,
            type = "bar",
            text = ~paste0(Quantidade, " (", Percentual, "%)"),
            textposition = "auto",
            marker = list(color = c("#2ecc71", "#e74c3c"))) %>%
      layout(
        title = paste("Fluxo de Óbitos em Idosos Residentes em", cidade),
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Número de Óbitos")
      )
  })

  # ========== DESTINOS DOS IDOSOS ==========
  output$destinos_idosos <- renderPlotly({
    req(sim_idosos())
    req(!is.na(codigo_ref()))

    cod <- codigo_ref()
    cidade <- cidade_ref()

    df <- sim_idosos() %>%
      filter(CODMUNRES == cod & CODMUNOCOR != cod & !is.na(CODMUNOCOR)) %>%
      group_by(CODMUNOCOR, CIDADE_OCORRENCIA) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    if (nrow(df) == 0) {
      plot_ly() %>% layout(title = paste("Nenhum idoso residente em", cidade, "morreu fora"))
    } else {
      plot_ly(df, x = ~Total, y = ~reorder(CIDADE_OCORRENCIA, Total), type = "bar", orientation = "h",
              marker = list(color = "#e74c3c")) %>%
        layout(title = paste("Cidades onde idosos residentes em", cidade, "faleceram"),
               xaxis = list(title = "Número de Óbitos"),
               yaxis = list(title = ""))
    }
  })

  output$causas_top_idosos <- renderPlotly({
    req(sim_idosos())
    df <- sim_idosos() %>%
      filter(CAUSA_COD != "Ignorado") %>%
      group_by(CAUSA_COD, CAUSA_DESCRICAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    plot_ly(df, x = ~Total, y = ~reorder(CAUSA_DESCRICAO, Total), type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = "Principais Causas de Morte em Idosos (60+)",
             xaxis = list(title = "Número de Óbitos"),
             yaxis = list(title = ""))
  })

  output$idade_idosos <- renderPlotly({
    req(sim_idosos())
    df <- sim_idosos() %>%
      filter(!is.na(IDADE_ANOS)) %>%
      group_by(IDADE_ANOS) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~IDADE_ANOS, y = ~Total, type = "bar",
            marker = list(color = "#f39c12")) %>%
      layout(title = "Distribuição de Idade dos Idosos (60+)",
             xaxis = list(title = "Idade (anos)"),
             yaxis = list(title = "Óbitos"))
  })

  # ========== ESTABELECIMENTOS ==========

  output$obitos_estabelecimento <- renderPlotly({
    req(sim_data())
    df <- sim_data() %>%
      filter(!is.na(CNES_NOME)) %>%
      group_by(CNES, CNES_NOME) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)

    plot_ly(df, x = ~Total, y = ~reorder(CNES_NOME, Total), type = "bar", orientation = "h",
            marker = list(color = "#f39c12")) %>%
      layout(title = "Óbitos por Estabelecimento",
             xaxis = list(title = "Óbitos"),
             yaxis = list(title = ""))
  })

  output$comparativo_estabelecimentos <- renderPlotly({
    req(sim_data())

    top5 <- sim_data() %>%
      filter(!is.na(CNES)) %>%
      group_by(CNES) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      top_n(5, Total) %>%
      pull(CNES)

    if (length(top5) > 0) {
      df <- sim_data() %>%
        filter(CNES %in% top5) %>%
        group_by(CNES_NOME, MES_NOME) %>%
        summarise(Total = n(), .groups = 'drop')

      plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~CNES_NOME,
              type = "scatter", mode = "lines+markers") %>%
        layout(title = "Comparativo Mensal - Top 5 Estabelecimentos",
               xaxis = list(title = "Mês"),
               yaxis = list(title = "Óbitos"))
    } else {
      plot_ly() %>% layout(title = "Sem dados suficientes para comparativo")
    }
  })

  output$tabela_estab_busca <- renderDT({
    req(sim_data())

    if (!is.null(input$buscar_estabelecimento) && input$buscar_estabelecimento != "") {
      busca <- toupper(input$buscar_estabelecimento)
      df <- sim_data() %>%
        filter(grepl(busca, toupper(CNES_NOME), ignore.case = TRUE)) %>%
        group_by(CNES, CNES_NOME) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))

      datatable(df, options = list(pageLength = 10), rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_estabelecimento))
    } else {
      datatable(data.frame(Mensagem = "Digite um nome para buscar"), rownames = FALSE)
    }
  })

  # ========== MAPA ==========

  output$mapa_obitos <- renderLeaflet({
    req(dados_mapa())

    df <- dados_mapa()

    if (nrow(df) == 0) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = -51.9, lat = -24.6, zoom = 6))
    }

    pal <- colorNumeric(palette = "RdYlBu", domain = df$total_obitos, reverse = TRUE)

    map <- leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        lng1 = min(df$longitude), lat1 = min(df$latitude),
        lng2 = max(df$longitude), lat2 = max(df$latitude)
      )

    if (input$mapa_tipo_sim == "pontos") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~sqrt(total_obitos) * 2,
          color = ~pal(total_obitos), fillOpacity = 0.7,
          stroke = TRUE, weight = 2,
          popup = ~popup_text, label = ~CNES_NOME,
          group = "Estabelecimentos"
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~total_obitos,
                  title = "Total de Óbitos", opacity = 0.7)

    } else if (input$mapa_tipo_sim == "heatmap") {
      map <- map %>%
        addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~total_obitos,
                   blur = 20, max = 0.05, radius = 15, group = "Heatmap") %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 6,
                         color = "black", fillOpacity = 0.3, stroke = TRUE, weight = 1,
                         popup = ~popup_text, label = ~CNES_NOME, group = "Marcadores")

    } else if (input$mapa_tipo_sim == "cluster") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude, radius = 8,
          color = "#e74c3c", fillOpacity = 0.6,
          popup = ~popup_text, label = ~CNES_NOME,
          clusterOptions = markerClusterOptions(
            spiderfyOnMaxZoom = TRUE,
            showCoverageOnHover = FALSE,
            zoomToBoundsOnClick = TRUE,
            disableClusteringAtZoom = 14
          )
        )
    }

    map
  })

  output$tabela_estabelecimentos_mapa <- renderDT({
    req(dados_mapa())

    df <- dados_mapa() %>%
      select(CNES_NOME, total_obitos, idade_media, perc_masculino, perc_feminino, obitos_60_mais) %>%
      arrange(desc(total_obitos))

    datatable(df,
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
              ),
              colnames = c("Estabelecimento", "Total", "Idade Média", "% Masc", "% Fem", "60+"),
              rownames = FALSE) %>%
      formatRound(columns = "idade_media", digits = 1)
  })

  # ========== DADOS BRUTOS ==========

  output$tabela_sim <- renderDT({
    req(sim_data())

    tabela <- sim_data() %>%
      select(CNES, CNES_NOME, DTOBITO, IDADE_ANOS, SEXO_LABEL, CAUSA_COD, CAUSA_DESCRICAO,
             CIDADE_RESIDENCIA, MES_NOME) %>%
      head(500)

    datatable(tabela,
              options = list(scrollX = TRUE, pageLength = 20),
              colnames = c("CNES", "Estabelecimento", "Data", "Idade", "Sexo", "CID", "Causa",
                           "Cidade Residência", "Mês"))
  })

  output$download_sim <- downloadHandler(
    filename = function() paste0("sim_", input$cidade_sim, "_", input$ano_sim, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(sim_data())
      write.csv(sim_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ========== RELATÓRIO ==========

  output$relatorio_sim <- renderUI({
    req(sim_data())

    cidade_nome <- if (is.null(cidade_ref())) "Todas as Cidades" else cidade_ref()
    total <- nrow(sim_data())
    media_idade <- round(mean(sim_data()$IDADE_ANOS, na.rm = TRUE), 1)
    perc_masc <- round(sum(sim_data()$SEXO_LABEL == "Masculino", na.rm = TRUE) / total * 100, 1)
    perc_fem <- round(sum(sim_data()$SEXO_LABEL == "Feminino", na.rm = TRUE) / total * 100, 1)
    perc_idosos <- round(sum(sim_data()$IDADE_ANOS >= 60, na.rm = TRUE) / total * 100, 1)

    top_causas <- sim_data() %>%
      filter(CAUSA_COD != "Ignorado") %>%
      group_by(CAUSA_DESCRICAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(5)

    causas_text <- paste0(top_causas$CAUSA_DESCRICAO, " (", top_causas$Total, ")", collapse = ", ")

    mes_txt <- if (input$mes_sim != "Todos") paste0(" | Mês: ", input$mes_sim) else ""

    HTML(paste0("
    <div style='font-family: Arial; padding: 20px;'>
      <h1 style='color: #c0392b;'>RELATÓRIO DE MORTALIDADE</h1>
      <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sim, mes_txt, "</h3>
      <hr/>

      <h2>1. METODOLOGIA</h2>
      <p>Dados do SIM (Sistema de Informação sobre Mortalidade) via DATASUS, lidos de arquivos Parquet gerados pelo Extrator DATASUS.</p>
      <p>CIDs traduzidos usando dicionário CID-10.</p>
      <p>Idade convertida conforme padrão SIM (400+ = 1-99 anos, 500+ = 100+ anos).</p>

      <h2>2. NÚMEROS GLOBAIS</h2>
      <ul>
        <li>Total de Óbitos: <strong>", format(total, big.mark = "."), "</strong></li>
        <li>Idade Média: <strong>", media_idade, " anos</strong></li>
        <li>Sexo Masculino: <strong>", perc_masc, "%</strong></li>
        <li>Sexo Feminino: <strong>", perc_fem, "%</strong></li>
        <li>Óbitos em Idosos (60+): <strong>", perc_idosos, "%</strong></li>
      </ul>

      <h2>3. PRINCIPAIS CAUSAS</h2>
      <p>", causas_text, "</p>

      <hr/>
      <p>Relatório gerado em ", Sys.Date(), "</p>
    </div>
    "))
  })

  output$download_relatorio_sim <- downloadHandler(
    filename = function() paste0("relatorio_sim_", input$cidade_sim, "_", input$ano_sim, "_", Sys.Date(), ".html"),
    content = function(file) {
      req(sim_data())
      cidade_nome <- if (is.null(cidade_ref())) "Todas" else cidade_ref()
      total <- nrow(sim_data())
      media_idade <- round(mean(sim_data()$IDADE_ANOS, na.rm = TRUE), 1)

      html <- paste0("
      <html>
      <head><meta charset='UTF-8'><style>body{font-family:Arial;margin:40px;}</style></head>
      <body>
        <h1>RELATÓRIO DE MORTALIDADE</h1>
        <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sim, "</h3>
        <hr/>
        <h2>Resumo</h2>
        <ul>
          <li>Total de Óbitos: ", format(total, big.mark = "."), "</li>
          <li>Idade Média: ", media_idade, " anos</li>
        </ul>
        <hr/>
        <p>Gerado em ", Sys.Date(), "</p>
      </body>
      </html>
      ")
      writeLines(html, file, useBytes = TRUE)
    }
  )
}

# ==============================================================================
# EXECUTAR
# ==============================================================================
onStop(function() {
  try(dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  try(dbDisconnect(con_dict, shutdown = TRUE), silent = TRUE)
})

shinyApp(ui = ui, server = server)