# DASHBOARD SIH (PARQUET) - COM FILTRO DE CIDADE E MAPA
# Sistema de Informação Hospitalar
# Mesmo modelo do DashSimParquet e DashSiaParquet: lê os Parquets gerados pelo Extrator
#   <pasta_app>/Tabelas_Todas/SIH/FINAL/FINAL_SIH-<VARIANTE>_<UF>_<CIDADE>_<ANO>_<MES>.parquet

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
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  if (sys.nframe() > 0) {
    for (i in rev(seq_len(sys.nframe()))) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nzchar(ofile)) {
        return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
      }
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

pasta_app <- obter_pasta_app()
pasta_raiz <- if (tolower(basename(pasta_app)) %in% c("extratorsih", "extradorsih")) {
  dirname(pasta_app)
} else {
  pasta_app
}

# ==============================================================================
# CONEXÕES DUCKDB
# ==============================================================================
con <- dbConnect(duckdb::duckdb())

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
    SELECT id_municipio_6 as id6, nome, sigla_uf
    FROM municipios
"))
municipios_df$id6 <- as.character(municipios_df$id6)

lookup_municipios <- setNames(municipios_df$nome, municipios_df$id6)

nome_municipio_vec <- function(cod) {
  cod <- as.character(cod)
  cod <- ifelse(is.na(cod) | cod == "", NA_character_,
                str_pad(substr(cod, 1, 6), 6, pad = "0"))
  nomes <- unname(lookup_municipios[cod])
  ifelse(is.na(nomes), cod, nomes)
}

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

# Dicionário CID-10
cid_dict <- tryCatch({
  d <- as.data.frame(dbGetQuery(con_dict, "SELECT subcat, descricao FROM cid10_subcategorias"))
  d$subcat <- as.character(d$subcat)
  distinct(d, subcat, .keep_all = TRUE)
}, error = function(e) {
  message("Dicionário CID não carregado: ", conditionMessage(e))
  data.frame(subcat = character(0), descricao = character(0), stringsAsFactors = FALSE)
})

# ==============================================================================
# CNES (coordenadas e nomes)
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
# CONFIGURAÇÃO DOS PARQUETS (<pasta_app>/Tabelas_Todas/SIH/FINAL)
# ==============================================================================
PATH_DADOS <- file.path(pasta_app, "Tabelas_Todas")

listar_parquets_sih <- function() {
  pastas <- file.path(PATH_DADOS, c("SIH", "SIH-RD", "SIH_RD", "SIHRD"), "FINAL")
  pastas <- pastas[dir.exists(pastas)]
  if (length(pastas) == 0) return(character(0))

  arquivos <- unlist(lapply(pastas, function(p) {
    list.files(p, pattern = "^FINAL_SIH.*\\.parquet$", full.names = TRUE, ignore.case = TRUE)
  }))
  if (length(arquivos) == 0) return(character(0))
  normalizePath(arquivos, winslash = "/", mustWork = FALSE)
}

# Formato:
#   FINAL_SIH-RD_UF_CIDADE_ANO_MES.parquet  (ex.: FINAL_SIH-RD_PR_Irati_2024_01.parquet)
#   FINAL_SIH-RJ_UF_CIDADE_ANO_MES.parquet
extrair_metadados_parquet <- function(caminho_arquivo) {
  nome <- basename(caminho_arquivo)
  nome_sem_ext <- gsub("\\.parquet$", "", nome, ignore.case = TRUE)
  partes <- strsplit(nome_sem_ext, "_")[[1]]

  resultado <- list(
    caminho = caminho_arquivo,
    sistema = NA_character_,
    uf = NA_character_,
    cidade = NA_character_,
    ano = NA_integer_,
    mes = NA_integer_
  )

  n <- length(partes)
  if (n >= 6) {
    resultado$sistema <- partes[2]
    resultado$uf <- partes[3]
    resultado$cidade <- paste(partes[4:(n - 2)], collapse = " ")

    ano_valor <- suppressWarnings(as.numeric(partes[n - 1]))
    if (!is.na(ano_valor) && ano_valor > 1900 && ano_valor < 2100) {
      resultado$ano <- ano_valor
    }

    mes_valor <- suppressWarnings(as.numeric(partes[n]))
    if (!is.na(mes_valor) && mes_valor >= 1 && mes_valor <= 12) {
      resultado$mes <- mes_valor
    }
  }

  resultado
}

metadados_parquets <- function() {
  arquivos <- listar_parquets_sih()
  if (length(arquivos) == 0) {
    return(data.frame(caminho = character(0), sistema = character(0),
                      uf = character(0), cidade = character(0),
                      ano = numeric(0), mes = numeric(0),
                      stringsAsFactors = FALSE))
  }
  md <- purrr::map_df(arquivos, function(x) {
    m <- extrair_metadados_parquet(x)
    data.frame(caminho = m$caminho, sistema = m$sistema, uf = m$uf,
               cidade = m$cidade, ano = m$ano, mes = m$mes,
               stringsAsFactors = FALSE)
  })
  md %>% filter(!is.na(ano), !is.na(cidade), cidade != "")
}

# ==============================================================================
# CIDADES E ANOS DISPONÍVEIS
# ==============================================================================
obter_cidades_disponiveis <- function() {
  md <- metadados_parquets()
  message("Parquets SIH encontrados: ", nrow(md))
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
# CONVERSÃO DE IDADE (PADRÃO DATASUS - SIH)
# ==============================================================================
converter_idade_sih <- function(idade_raw, cod_idade) {
  idade <- suppressWarnings(as.numeric(as.character(idade_raw)))
  cod   <- as.character(cod_idade)

  idade_anos <- case_when(
    is.na(idade) ~ NA_real_,
    cod == "1" ~ idade,                  # Anos
    cod == "2" ~ idade / 12,             # Meses
    cod == "3" ~ idade / 365,            # Dias
    cod == "4" ~ idade / 8760,           # Horas
    TRUE ~ NA_real_
  )

  idade_anos <- floor(idade_anos)
  idade_anos <- ifelse(idade_anos > 120 | idade_anos < 0, NA, idade_anos)
  idade_anos
}

# ==============================================================================
# CARREGAR DADOS SIH COM DUCKDB (PARQUET)
# ==============================================================================
carregar_sih_parquet <- function(cidade_filtro = NULL, ano_filtro = NULL, mes_filtro = NULL) {

  metadados <- metadados_parquets()

  if (nrow(metadados) == 0) {
    showNotification("Nenhum arquivo Parquet encontrado! Extraia dados primeiro.", type = "error")
    return(NULL)
  }

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

  caminhos_sql <- paste0("'", gsub("'", "''", metadados$caminho), "'", collapse = ", ")

  query <- sprintf(
    "SELECT * FROM read_parquet([%s], union_by_name = true, filename = true)",
    caminhos_sql
  )

  df <- dbGetQuery(con, query)

  if (nrow(df) == 0) return(NULL)

  names(df) <- toupper(names(df))

  # CIDADE_ORIGEM por arquivo
  md_map <- metadados %>%
    mutate(caminho_n = normalizePath(caminho, winslash = "/", mustWork = FALSE)) %>%
    select(caminho_n, CIDADE_ORIGEM = cidade, UF_ORIGEM = uf, SISTEMA = sistema)
  df$FILENAME <- normalizePath(df$FILENAME, winslash = "/", mustWork = FALSE)
  df <- df %>%
    left_join(md_map, by = c("FILENAME" = "caminho_n")) %>%
    select(-FILENAME)

  pega_col <- function(opcoes) {
    achou <- intersect(names(df), opcoes)[1]
    if (is.na(achou)) rep(NA, nrow(df)) else df[[achou]]
  }

  # ---------------- Mês (SIH: MES_CMPT) ----------------
  mes <- suppressWarnings(as.numeric(as.character(pega_col(c("MES_CMPT", "MES")))))
  df$MES <- ifelse(is.na(mes) | mes < 1 | mes > 12, 1, mes)
  df$MES_NOME <- factor(df$MES, levels = 1:12,
                        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                   "Jul", "Ago", "Set", "Out", "Nov", "Dez"))

  # ---------------- Idade (SIH: IDADE + COD_IDADE) ----------------
  idade_raw <- pega_col(c("IDADE"))
  cod_idade <- pega_col(c("COD_IDADE"))
  df$IDADE_ANOS <- converter_idade_sih(idade_raw, cod_idade)

  # ---------------- Sexo (SIH: 1=Masculino, 3=Feminino) ----------------
  sexo <- as.character(pega_col(c("SEXO")))
  df$SEXO_LABEL <- case_when(
    sexo == "1" ~ "Masculino",
    sexo == "2" ~ "Feminino",
    sexo == "3" ~ "Feminino",
    TRUE ~ "Ignorado"
  )

  # ---------------- CID ----------------
  cid_raw <- as.character(pega_col(c("DIAG_PRINC", "CID10", "CID", "CID_PRINC")))
  df$CID <- ifelse(is.na(cid_raw) | cid_raw == "", "Ignorado", substr(cid_raw, 1, 4))

  df <- df %>%
    left_join(cid_dict, by = c("CID" = "subcat")) %>%
    mutate(CID_DESCRICAO = ifelse(is.na(descricao), CID, descricao)) %>%
    select(-descricao)

  # ---------------- CNES (estabelecimento) ----------------
  cnes_raw <- as.character(pega_col(c("CNES", "CO_CNES", "CGC_HOSP")))
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
  df$MUNIC_RES <- str_pad(as.character(pega_col(c("MUNIC_RES", "CODMUNRES"))), 6, pad = "0")
  df$MUNIC_MOV <- str_pad(as.character(pega_col(c("MUNIC_MOV", "MUNIC_OCOR"))), 6, pad = "0")

  df$CIDADE_RESIDENCIA <- nome_municipio_vec(df$MUNIC_RES)
  df$CIDADE_INTERNACAO <- nome_municipio_vec(df$MUNIC_MOV)

  # ---------------- Diárias / Valor / Óbito ----------------
  df$DIARIAS <- suppressWarnings(as.numeric(as.character(
    pega_col(c("QT_DIARIAS", "DIARIAS", "DIARIA")))))
  df$DIARIAS <- ifelse(is.na(df$DIARIAS), 1, df$DIARIAS)

  df$VALOR <- suppressWarnings(as.numeric(as.character(
    pega_col(c("VAL_TOT", "VALOR_TOT", "VALOR", "VAL_SH")))))
  df$VALOR <- ifelse(is.na(df$VALOR), 0, df$VALOR)

  obito_raw <- suppressWarnings(as.numeric(as.character(
    pega_col(c("MORTE", "OBITO", "IND_OBITO", "COD_OBITO")))))
  df$OBITO <- ifelse(!is.na(obito_raw) & obito_raw == 1, 1, 0)

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
      df$MUNIC_RES == codigo_ref & df$MUNIC_MOV == codigo_ref ~ paste("Reside e Interna em", cidade_ref),
      df$MUNIC_RES == codigo_ref & df$MUNIC_MOV != codigo_ref ~ paste("Apenas Reside em", cidade_ref),
      df$MUNIC_RES != codigo_ref & df$MUNIC_MOV == codigo_ref ~ paste("Apenas Interna em", cidade_ref),
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

      selectInput("cidade_sih", tags$b(icon("city"), " Cidade:"),
                  choices = NULL, selected = NULL),

      selectInput("ano_sih", "Selecione o Ano:", choices = NULL),

      selectInput("mes_sih", "Selecione o Mês:",
                  choices = c("Todos", "Janeiro" = 1, "Fevereiro" = 2, "Março" = 3, "Abril" = 4,
                              "Maio" = 5, "Junho" = 6, "Julho" = 7, "Agosto" = 8,
                              "Setembro" = 9, "Outubro" = 10, "Novembro" = 11, "Dezembro" = 12),
                  selected = "Todos"),

      numericInput("pop_ref", "População de referência:", value = 60000, min = 1, step = 1000),
      hr(),
      menuItem("Visão Geral", tabName = "geral", icon = icon("tachometer-alt")),
      menuItem("Internações", tabName = "internacoes", icon = icon("procedures")),
      menuItem("Causas (CID)", tabName = "causas", icon = icon("heartbeat")),
      menuItem("Análise de Fluxo", tabName = "fluxo", icon = icon("exchange-alt")),
      menuItem("Estabelecimentos", tabName = "estabelecimentos", icon = icon("hospital")),
      menuItem("🗺️ Mapa das Internações", tabName = "mapa", icon = icon("map-marker-alt")),
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
          valueBoxOutput("total_internacoes", width = 3),
          valueBoxOutput("total_diarias", width = 3),
          valueBoxOutput("media_diarias", width = 3),
          valueBoxOutput("taxa_internacao", width = 3)
        ),
        fluidRow(
          valueBoxOutput("total_custo", width = 3),
          valueBoxOutput("custo_medio", width = 3),
          valueBoxOutput("obitos_hospitalares", width = 3),
          valueBoxOutput("taxa_mortalidade", width = 3)
        ),
        fluidRow(
          box(title = "Internações por Mês", width = 12, status = "primary",
              plotlyOutput("evolucao_mensal_sih", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Top 10 Causas de Internação (CID traduzido)", width = 12, status = "success",
              plotlyOutput("top_causas_sih", height = "500px")
          )
        )
      ),

      # ========== INTERNAÇÕES ==========
      tabItem(
        tabName = "internacoes",
        fluidRow(
          box(title = "Distribuição por Sexo", width = 6, status = "info",
              plotlyOutput("sexo_internacao", height = "400px")
          ),
          box(title = "Distribuição por Faixa Etária", width = 6, status = "info",
              plotlyOutput("idade_internacao", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Evolução por Sexo", width = 12, status = "info",
              plotlyOutput("evolucao_sexo", height = "400px")
          )
        )
      ),

      # ========== CAUSAS (CID) ==========
      tabItem(
        tabName = "causas",
        fluidRow(
          box(title = "Principais Causas de Internação (CID traduzido)", width = 12, status = "danger",
              plotlyOutput("causas_com_traducao", height = "600px")
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

      # ========== FLUXO ==========
      tabItem(
        tabName = "fluxo",
        fluidRow(
          box(title = "📊 Resumo do Fluxo de Pacientes", width = 12, status = "info",
              div(style = "font-size: 16px;", uiOutput("resumo_fluxo"))
          )
        ),
        fluidRow(
          box(title = "Fluxograma: Residência vs Internação", width = 12, status = "primary",
              plotlyOutput("fluxo_sankey", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Municípios de Residência (internados na cidade)", width = 6, status = "warning",
              plotlyOutput("municipios_atendidos", height = "500px")
          ),
          box(title = "Destinos dos Residentes (internações fora)", width = 6, status = "warning",
              plotlyOutput("destinos_residentes", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Evolução do Fluxo por Mês", width = 12, status = "danger",
              plotlyOutput("evolucao_fluxo", height = "500px")
          )
        )
      ),

      # ========== ESTABELECIMENTOS ==========
      tabItem(
        tabName = "estabelecimentos",
        fluidRow(
          box(title = "Internações por Estabelecimento (com nomes CNES)", width = 12, status = "warning",
              plotlyOutput("internacoes_estabelecimentos", height = "600px")
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

      # ========== MAPA ==========
      tabItem(
        tabName = "mapa",
        fluidRow(
          box(
            title = "🗺️ Localização das Internações por Estabelecimento",
            width = 12,
            status = "danger",
            solidHeader = TRUE,
            div(
              style = "margin-bottom: 15px;",
              radioButtons("mapa_tipo_sih", "Tipo de Visualização:",
                           choices = c(
                             "📍 Pontos" = "pontos",
                             "🔥 Heatmap" = "heatmap",
                             "🔵 Cluster" = "cluster"
                           ),
                           inline = TRUE
              )),
            leafletOutput("mapa_sih", height = "600px")
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
          box(title = "Dados SIH com Tradução CID e Nomes CNES", width = 12, status = "primary",
              DTOutput("tabela_sih"),
              br(),
              downloadButton("download_sih", "📥 Baixar Dados (CSV)", class = "btn-success")
          )
        )
      ),

      # ========== RELATÓRIO ==========
      tabItem(
        tabName = "relatorio",
        fluidRow(
          box(title = "Relatório de Internações Hospitalares", width = 12, status = "success",
              downloadButton("download_relatorio_sih", "📥 Baixar Relatório (HTML)", class = "btn-success"),
              br(), br(),
              htmlOutput("relatorio_sih")
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

  populacao <- reactive({
    p <- input$pop_ref
    if (is.null(p) || is.na(p) || p <= 0) 60000 else p
  })

  # ========== CARREGAR CIDADES DISPONÍVEIS ==========
  observe({
    cidades <- obter_cidades_disponiveis()

    if (length(cidades) > 0) {
      opcoes <- c("Todas", cidades)
      updateSelectInput(session, "cidade_sih", choices = opcoes, selected = "Todas")
    } else {
      updateSelectInput(session, "cidade_sih",
                        choices = c("Nenhum dado encontrado" = ""), selected = "")
      showNotification("Nenhum arquivo Parquet do SIH encontrado! Use o Extrator DATASUS primeiro.",
                       type = "error", duration = 10)
    }
  })

  # ========== CARREGAR ANOS DISPONÍVEIS ==========
  observeEvent(input$cidade_sih, {
    req(input$cidade_sih != "", input$cidade_sih != "Nenhum dado encontrado")

    cidade_param <- if (input$cidade_sih == "Todas") NULL else input$cidade_sih
    anos <- obter_anos_disponiveis(cidade_param)

    if (length(anos) > 0) {
      updateSelectInput(session, "ano_sih", choices = anos, selected = anos[1])
    } else {
      updateSelectInput(session, "ano_sih",
                        choices = c("Nenhum ano encontrado" = ""), selected = "")
    }
  })

  # ========== TÍTULO ==========
  output$titulo_dashboard <- renderUI({
    if (is.null(input$cidade_sih) || input$cidade_sih == "" || input$cidade_sih == "Todas") {
      paste("SIH - Sistema de Informação Hospitalar - Todas as Cidades")
    } else {
      paste("SIH - Sistema de Informação Hospitalar -", input$cidade_sih)
    }
  })

  # ========== CIDADE REFERÊNCIA ==========
  cidade_ref <- reactive({
    if (is.null(input$cidade_sih) || input$cidade_sih %in% c("", "Todas")) NULL else input$cidade_sih
  })

  codigo_ref <- reactive({
    codigo_municipio(cidade_ref())
  })

  # ========== CARREGAR DADOS ==========
  sih_data <- reactive({
    req(input$ano_sih, input$cidade_sih,
        input$cidade_sih != "", input$cidade_sih != "Nenhum dado encontrado")
    req(input$ano_sih != "Nenhum ano encontrado")

    cidade_param <- if (input$cidade_sih == "Todas") NULL else input$cidade_sih

    df <- carregar_sih_parquet(
      cidade_filtro = cidade_param,
      ano_filtro = as.numeric(input$ano_sih),
      mes_filtro = input$mes_sih
    )

    if (is.null(df) || nrow(df) == 0) {
      showNotification("Nenhum dado encontrado para os filtros selecionados.", type = "warning")
      return(NULL)
    }

    df
  })

  # ========== DADOS MAPA ==========
  dados_mapa <- reactive({
    req(sih_data())

    df <- sih_data() %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      group_by(CNES, CNES_NOME, latitude, longitude) %>%
      summarise(
        total_internacoes = n(),
        media_diarias = round(mean(DIARIAS, na.rm = TRUE), 1),
        custo_total = sum(VALOR, na.rm = TRUE),
        obitos = sum(OBITO == 1, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        popup_text = paste0(
          "<b>", CNES_NOME, "</b><br>",
          "🏥 Internações: ", total_internacoes, "<br>",
          "📅 Diárias médias: ", media_diarias, "<br>",
          "💰 Custo total: R$ ", format(custo_total, big.mark = ".", decimal.mark = ","), "<br>",
          "💀 Óbitos: ", obitos, " (", round(obitos / total_internacoes * 100, 1), "%)"
        )
      )

    df
  })

  # ========== VALUE BOXES ==========
  output$total_internacoes <- renderValueBox({
    req(sih_data())
    cidade_nome <- if (is.null(cidade_ref())) "Todas as Cidades" else cidade_ref()
    valueBox(
      value = format(nrow(sih_data()), big.mark = "."),
      subtitle = paste("Total de Internações -", cidade_nome),
      icon = icon("procedures"),
      color = "blue"
    )
  })

  output$total_diarias <- renderValueBox({
    req(sih_data())
    total <- sum(sih_data()$DIARIAS, na.rm = TRUE)
    valueBox(
      value = format(total, big.mark = "."),
      subtitle = "Total de Diárias",
      icon = icon("calendar"),
      color = "green"
    )
  })

  output$media_diarias <- renderValueBox({
    req(sih_data())
    media <- round(mean(sih_data()$DIARIAS, na.rm = TRUE), 1)
    valueBox(
      value = media,
      subtitle = "Média de Diárias por Internação",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  output$taxa_internacao <- renderValueBox({
    req(sih_data(), populacao())
    taxa <- round(nrow(sih_data()) / populacao() * 1000, 2)
    valueBox(
      value = paste(taxa, "/1.000 hab"),
      subtitle = "Taxa de Internação",
      icon = icon("users"),
      color = "light-blue"
    )
  })

  output$total_custo <- renderValueBox({
    req(sih_data())
    total <- sum(sih_data()$VALOR, na.rm = TRUE)
    valueBox(
      value = paste("R$", format(total, big.mark = ".", decimal.mark = ",")),
      subtitle = "Custo Total",
      icon = icon("money"),
      color = "red"
    )
  })

  output$custo_medio <- renderValueBox({
    req(sih_data())
    media <- round(mean(sih_data()$VALOR, na.rm = TRUE), 2)
    valueBox(
      value = paste("R$", format(media, big.mark = ".", decimal.mark = ",")),
      subtitle = "Custo Médio por Internação",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  output$obitos_hospitalares <- renderValueBox({
    req(sih_data())
    obitos <- sum(sih_data()$OBITO == 1, na.rm = TRUE)
    valueBox(
      value = obitos,
      subtitle = "Óbitos Hospitalares",
      icon = icon("skull"),
      color = "red"
    )
  })

  output$taxa_mortalidade <- renderValueBox({
    req(sih_data())
    obitos <- sum(sih_data()$OBITO == 1, na.rm = TRUE)
    taxa <- round(obitos / nrow(sih_data()) * 100, 2)
    valueBox(
      value = paste(taxa, "%"),
      subtitle = "Mortalidade Hospitalar",
      icon = icon("heartbeat"),
      color = "red"
    )
  })

  # ========== GRÁFICOS ==========
  output$evolucao_mensal_sih <- renderPlotly({
    req(sih_data())
    df <- sih_data() %>%
      group_by(MES, MES_NOME) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 2),
            marker = list(color = "#2980b9", size = 8)) %>%
      layout(title = paste("Internações por Mês -", input$ano_sih),
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Internações"))
  })

  output$top_causas_sih <- renderPlotly({
    req(sih_data())
    df <- sih_data() %>%
      filter(CID != "Ignorado") %>%
      group_by(CID, CID_DESCRICAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    plot_ly(df, x = ~Total, y = ~reorder(CID_DESCRICAO, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#2ecc71")) %>%
      layout(title = "Top 10 Causas de Internação",
             xaxis = list(title = "Internações"),
             yaxis = list(title = ""))
  })

  # ========== INTERNAÇÕES ==========
  output$sexo_internacao <- renderPlotly({
    req(sih_data())
    df <- sih_data() %>%
      group_by(SEXO_LABEL) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, labels = ~SEXO_LABEL, values = ~Total, type = "pie",
            marker = list(colors = c("#3498db", "#e74c3c", "#95a5a6"))) %>%
      layout(title = "Internações por Sexo")
  })

  output$idade_internacao <- renderPlotly({
    req(sih_data())
    ordem <- c("< 1 ano", "1-14 anos", "15-29 anos", "30-44 anos",
               "45-59 anos", "60-74 anos", "75+ anos", "Ignorado")
    df <- sih_data() %>%
      group_by(FAIXA_IDADE) %>%
      summarise(Total = n(), .groups = 'drop')
    df$FAIXA_IDADE <- factor(df$FAIXA_IDADE, levels = ordem)
    df <- df %>% arrange(FAIXA_IDADE)

    plot_ly(df, x = ~FAIXA_IDADE, y = ~Total, type = "bar",
            marker = list(color = "#9b59b6")) %>%
      layout(title = "Internações por Faixa Etária",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Internações"))
  })

  output$evolucao_sexo <- renderPlotly({
    req(sih_data())
    df <- sih_data() %>%
      group_by(MES_NOME, SEXO_LABEL) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~SEXO_LABEL,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = "Evolução Mensal por Sexo",
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Internações"))
  })

  # ========== CAUSAS (CID) ==========
  output$causas_com_traducao <- renderPlotly({
    req(sih_data())
    df <- sih_data() %>%
      filter(CID != "Ignorado") %>%
      group_by(CID, CID_DESCRICAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)

    plot_ly(df, x = ~Total, y = ~reorder(CID_DESCRICAO, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = paste("Principais Causas de Internação -", input$ano_sih),
             xaxis = list(title = "Internações"),
             yaxis = list(title = ""))
  })

  output$tabela_cid_busca <- renderDT({
    req(sih_data())

    if (!is.null(input$buscar_cid) && input$buscar_cid != "") {
      busca <- toupper(input$buscar_cid)
      df <- sih_data() %>%
        filter(grepl(busca, CID, ignore.case = TRUE) |
               grepl(busca, CID_DESCRICAO, ignore.case = TRUE)) %>%
        group_by(CID, CID_DESCRICAO) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))

      datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_cid))
    } else {
      datatable(data.frame(Mensagem = "Digite um código CID para buscar"),
                rownames = FALSE, options = list(dom = 't'))
    }
  })

  # ========== FLUXO ==========
  output$resumo_fluxo <- renderUI({
    req(sih_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      return(div(style = "padding: 20px;",
                 h3("Selecione uma cidade para ver o fluxo de pacientes.")))
    }

    cidade <- cidade_ref()
    df_cidade <- sih_data() %>% filter(MUNIC_RES == cod | MUNIC_MOV == cod)

    reside <- sum(df_cidade$MUNIC_RES == cod, na.rm = TRUE)
    interna <- sum(df_cidade$MUNIC_MOV == cod, na.rm = TRUE)
    ambos <- sum(df_cidade$MUNIC_RES == cod & df_cidade$MUNIC_MOV == cod, na.rm = TRUE)
    apenas_reside <- reside - ambos
    apenas_interna <- interna - ambos
    saldo <- apenas_interna - apenas_reside

    div(
      style = "display: flex; justify-content: space-around; text-align: center; flex-wrap: wrap;",
      div(style = "background: #2ecc71; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(reside, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Residentes em", cidade)),
          p(style = "margin: 0; color: white; font-size: 12px;", "Moradores internados")
      ),
      div(style = "background: #3498db; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(interna, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Internações em", cidade)),
          p(style = "margin: 0; color: white; font-size: 12px;", "Atendimentos realizados")
      ),
      div(style = "background: #f39c12; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(apenas_interna, big.mark = ".")),
          p(style = "margin: 0; color: white;", "Pacientes de Fora"),
          p(style = "margin: 0; color: white; font-size: 12px;", paste("Atendidos em", cidade))
      ),
      div(style = "background: #e74c3c; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(apenas_reside, big.mark = ".")),
          p(style = "margin: 0; color: white;", "Residentes Fora"),
          p(style = "margin: 0; color: white; font-size: 12px;", "Internaram em outros municípios")
      ),
      div(style = sprintf("background: %s; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
                          ifelse(saldo > 0, "#27ae60", "#c0392b")),
          h3(style = "margin: 0; color: white;", format(abs(saldo), big.mark = ".")),
          p(style = "margin: 0; color: white;", ifelse(saldo > 0, "Superávit", "Déficit")),
          p(style = "margin: 0; color: white; font-size: 12px;",
            ifelse(saldo > 0, "Atende mais do que perde", "Perde mais do que atende"))
      )
    )
  })

  output$fluxo_sankey <- renderPlotly({
    req(sih_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) return(plot_ly() %>% layout(title = "Selecione uma cidade"))

    cidade <- cidade_ref()
    df_filtrado <- sih_data() %>% filter(MUNIC_RES == cod | MUNIC_MOV == cod)

    reside <- df_filtrado$MUNIC_RES == cod
    interna <- df_filtrado$MUNIC_MOV == cod

    valores <- c(
      sum(reside & interna, na.rm = TRUE),
      sum(reside & !interna, na.rm = TRUE),
      sum(!reside & interna, na.rm = TRUE),
      sum(!reside & !interna, na.rm = TRUE)
    )

    plot_ly(type = "sankey", orientation = "h",
            node = list(
              label = c(paste("Reside em", cidade), "Reside fora",
                        paste("Interna em", cidade), "Interna fora"),
              color = c("#2ecc71", "#e74c3c", "#3498db", "#f39c12"),
              pad = 15, thickness = 20
            ),
            link = list(source = c(0, 0, 1, 1), target = c(2, 3, 2, 3), value = valores)) %>%
      layout(title = paste("Fluxo de Pacientes:", cidade),
             font = list(size = 12))
  })

  output$municipios_atendidos <- renderPlotly({
    req(sih_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) return(plot_ly() %>% layout(title = "Selecione uma cidade"))

    cidade <- cidade_ref()
    df <- sih_data() %>%
      filter(MUNIC_MOV == cod & MUNIC_RES != cod & !is.na(MUNIC_RES)) %>%
      group_by(MUNIC_RES, CIDADE_RESIDENCIA) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = paste("Nenhum paciente de fora internou em", cidade)))
    }

    plot_ly(df, x = ~Total, y = ~reorder(CIDADE_RESIDENCIA, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#f39c12")) %>%
      layout(title = paste("Municípios de Residência (internados em", cidade, ")"),
             xaxis = list(title = "Internações"),
             yaxis = list(title = ""))
  })

  output$destinos_residentes <- renderPlotly({
    req(sih_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) return(plot_ly() %>% layout(title = "Selecione uma cidade"))

    cidade <- cidade_ref()
    df <- sih_data() %>%
      filter(MUNIC_RES == cod & MUNIC_MOV != cod & !is.na(MUNIC_MOV)) %>%
      group_by(MUNIC_MOV, CIDADE_INTERNACAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = paste("Nenhum residente de", cidade, "internou fora")))
    }

    plot_ly(df, x = ~Total, y = ~reorder(CIDADE_INTERNACAO, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = paste("Destinos dos Residentes de", cidade, "(internações fora)"),
             xaxis = list(title = "Internações"),
             yaxis = list(title = ""))
  })

  output$evolucao_fluxo <- renderPlotly({
    req(sih_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) return(plot_ly() %>% layout(title = "Selecione uma cidade"))

    df <- sih_data() %>%
      filter(MUNIC_RES == cod | MUNIC_MOV == cod) %>%
      group_by(MES_NOME, TIPO_FLUXO) %>%
      summarise(Total = n(), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~TIPO_FLUXO,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = "Evolução Mensal do Fluxo de Pacientes",
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Internações"))
  })

  # ========== ESTABELECIMENTOS ==========
  output$internacoes_estabelecimentos <- renderPlotly({
    req(sih_data())
    df <- sih_data() %>%
      filter(!is.na(CNES_NOME)) %>%
      group_by(CNES, CNES_NOME) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)

    plot_ly(df, x = ~Total, y = ~reorder(CNES_NOME, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#f39c12")) %>%
      layout(title = "Internações por Estabelecimento",
             xaxis = list(title = "Internações"),
             yaxis = list(title = ""))
  })

  output$comparativo_estabelecimentos <- renderPlotly({
    req(sih_data())
    top5 <- sih_data() %>%
      filter(!is.na(CNES)) %>%
      group_by(CNES) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      slice_max(Total, n = 5) %>%
      pull(CNES)

    if (length(top5) > 0) {
      df <- sih_data() %>%
        filter(CNES %in% top5) %>%
        group_by(CNES_NOME, MES_NOME) %>%
        summarise(Total = n(), .groups = 'drop')

      plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~CNES_NOME,
              type = "scatter", mode = "lines+markers") %>%
        layout(title = "Comparativo Mensal - Top 5 Estabelecimentos",
               xaxis = list(title = "Mês"),
               yaxis = list(title = "Internações"))
    } else {
      plot_ly() %>% layout(title = "Sem dados suficientes para comparativo")
    }
  })

  output$tabela_estab_busca <- renderDT({
    req(sih_data())

    if (!is.null(input$buscar_estabelecimento) && input$buscar_estabelecimento != "") {
      busca <- toupper(input$buscar_estabelecimento)
      df <- sih_data() %>%
        filter(grepl(busca, toupper(CNES_NOME), ignore.case = TRUE)) %>%
        group_by(CNES, CNES_NOME) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))

      datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_estabelecimento))
    } else {
      datatable(data.frame(Mensagem = "Digite um nome para buscar"),
                rownames = FALSE, options = list(dom = 't'))
    }
  })

  # ========== MAPA ==========
  output$mapa_sih <- renderLeaflet({
    req(dados_mapa())

    df <- dados_mapa()

    if (nrow(df) == 0) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = -51.9, lat = -24.6, zoom = 6))
    }

    pal <- colorNumeric(palette = "RdYlBu", domain = df$total_internacoes, reverse = TRUE)

    map <- leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        lng1 = min(df$longitude), lat1 = min(df$latitude),
        lng2 = max(df$longitude), lat2 = max(df$latitude)
      )

    if (input$mapa_tipo_sih == "pontos") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~sqrt(total_internacoes) * 2,
          color = ~pal(total_internacoes), fillOpacity = 0.7,
          stroke = TRUE, weight = 2,
          popup = ~popup_text, label = ~CNES_NOME,
          group = "Estabelecimentos"
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~total_internacoes,
                  title = "Total de Internações", opacity = 0.7)

    } else if (input$mapa_tipo_sih == "heatmap") {
      map <- map %>%
        addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~total_internacoes,
                   blur = 20, max = 0.05, radius = 15, group = "Heatmap") %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 6,
                         color = "black", fillOpacity = 0.3, stroke = TRUE, weight = 1,
                         popup = ~popup_text, label = ~CNES_NOME, group = "Marcadores")

    } else if (input$mapa_tipo_sih == "cluster") {
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
      select(CNES_NOME, total_internacoes, media_diarias, custo_total, obitos) %>%
      arrange(desc(total_internacoes))

    datatable(df,
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
              ),
              colnames = c("Estabelecimento", "Internações", "Média Diárias", "Custo Total (R$)", "Óbitos"),
              rownames = FALSE) %>%
      formatCurrency(columns = "custo_total", currency = "R$ ", digits = 2)
  })

  # ========== DADOS BRUTOS ==========
  output$tabela_sih <- renderDT({
    req(sih_data())

    tabela <- sih_data() %>%
      select(CNES, CNES_NOME, CID, CID_DESCRICAO, DIARIAS, VALOR,
             IDADE_ANOS, SEXO_LABEL, OBITO, MES_NOME,
             CIDADE_RESIDENCIA, CIDADE_INTERNACAO, TIPO_FLUXO) %>%
      head(500)

    datatable(as.data.frame(tabela),
              options = list(scrollX = TRUE, pageLength = 20),
              rownames = FALSE,
              colnames = c("CNES", "Estabelecimento", "CID", "Descrição CID",
                           "Diárias", "Valor (R$)", "Idade", "Sexo", "Óbito",
                           "Mês", "Cidade Residência", "Cidade Internação", "Fluxo"))
  })

  output$download_sih <- downloadHandler(
    filename = function() paste0("sih_", input$cidade_sih, "_", input$ano_sih, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(sih_data())
      write.csv(sih_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ========== RELATÓRIO ==========
  output$relatorio_sih <- renderUI({
    req(sih_data())

    cidade_nome <- if (is.null(cidade_ref())) "Todas as Cidades" else cidade_ref()
    total <- nrow(sih_data())
    diarias <- sum(sih_data()$DIARIAS, na.rm = TRUE)
    custo_total <- sum(sih_data()$VALOR, na.rm = TRUE)
    obitos <- sum(sih_data()$OBITO == 1, na.rm = TRUE)
    cod <- codigo_ref()

    residentes <- if (is.na(cod)) NA else sum(sih_data()$MUNIC_RES == cod, na.rm = TRUE)
    internacoes_cidade <- if (is.na(cod)) NA else sum(sih_data()$MUNIC_MOV == cod, na.rm = TRUE)
    pacientes_fora <- if (is.na(cod)) NA else
      sum(sih_data()$MUNIC_MOV == cod & sih_data()$MUNIC_RES != cod, na.rm = TRUE)

    top_causas <- sih_data() %>%
      filter(CID != "Ignorado") %>%
      group_by(CID_DESCRICAO) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(5)

    causas_text <- paste(paste0(top_causas$CID_DESCRICAO, " (", top_causas$Total, ")"),
                         collapse = ", ")

    mes_txt <- if (input$mes_sih != "Todos") paste0(" | Mês: ", input$mes_sih) else ""

    html_fluxo <- if (!is.na(cod)) {
      paste0(
        "<ul>",
        "<li>Residentes em ", cidade_nome, ": <strong>", format(residentes, big.mark = "."), "</strong></li>",
        "<li>Internações em ", cidade_nome, ": <strong>", format(internacoes_cidade, big.mark = "."), "</strong></li>",
        "<li>Pacientes de fora atendidos: <strong>", format(pacientes_fora, big.mark = "."), "</strong></li>",
        "</ul>"
      )
    } else ""

    HTML(paste0("
    <div style='font-family: Arial; padding: 20px;'>
      <h1 style='color: #3498db;'>RELATÓRIO DE INTERNAÇÕES HOSPITALARES</h1>
      <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sih, mes_txt, "</h3>
      <hr/>

      <h2>1. METODOLOGIA</h2>
      <p>Dados do SIH (Sistema de Informação Hospitalar) via DATASUS, lidos de arquivos Parquet gerados pelo Extrator DATASUS.</p>
      <p>CIDs traduzidos via dicionário CID-10.</p>
      <p>Municípios traduzidos via DuckDB. Estabelecimentos identificados via CNES.</p>
      <p>Idade convertida conforme padrão DATASUS (COD_IDADE: 1=Anos, 2=Meses, 3=Dias, 4=Horas).</p>

      <h2>2. NÚMEROS GLOBAIS</h2>
      <ul>
        <li>Total de Internações: <strong>", format(total, big.mark = "."), "</strong></li>
        <li>Total de Diárias: <strong>", format(diarias, big.mark = "."), "</strong></li>
        <li>Custo Total: <strong>R$ ", format(custo_total, big.mark = ".", decimal.mark = ","), "</strong></li>
        <li>Óbitos Hospitalares: <strong>", obitos, "</strong></li>
      </ul>

      <h2>3. ANÁLISE DE FLUXO</h2>
      ", html_fluxo, "

      <h2>4. PRINCIPAIS CAUSAS</h2>
      <p>", causas_text, "</p>

      <hr/>
      <p>Relatório gerado em ", Sys.Date(), "</p>
    </div>
    "))
  })

  output$download_relatorio_sih <- downloadHandler(
    filename = function() paste0("relatorio_sih_", input$cidade_sih, "_", input$ano_sih, "_", Sys.Date(), ".html"),
    content = function(file) {
      req(sih_data())
      cidade_nome <- if (is.null(cidade_ref())) "Todas" else cidade_ref()
      total <- nrow(sih_data())
      custo_total <- sum(sih_data()$VALOR, na.rm = TRUE)

      html <- paste0(
        "<html><head><meta charset='UTF-8'><style>body{font-family:Arial;margin:40px;}</style></head>",
        "<body>",
        "<h1>RELATÓRIO DE INTERNAÇÕES HOSPITALARES - ", cidade_nome, "</h1>",
        "<h3>Ano: ", input$ano_sih, "</h3><hr/>",
        "<h2>Resumo</h2>",
        "<ul><li>Total de Internações: ", format(total, big.mark = "."), "</li>",
        "<li>Custo Total: R$ ", format(custo_total, big.mark = ".", decimal.mark = ","), "</li></ul>",
        "<hr/><p>Gerado em ", Sys.Date(), "</p>",
        "</body></html>"
      )
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