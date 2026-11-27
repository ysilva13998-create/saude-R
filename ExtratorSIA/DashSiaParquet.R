# DASHBOARD SIA (PARQUET) - COM FILTRO DE CIDADE E MAPA
# Sistema de Informação Ambulatorial
# Mesmo modelo do DashSimParquet: lê os Parquets gerados pelo Extrator
#   <pasta_app>/Tabelas_Todas/SIA/FINAL/FINAL_SIA-<VARIANTE>_<UF>_<CIDADE>_<ANO>_<MES>.parquet

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

  # 2. Execução via source()
  if (sys.nframe() > 0) {
    for (i in rev(seq_len(sys.nframe()))) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nzchar(ofile)) {
        return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
      }
    }
  }

  # 3. Fallback
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

# ==============================================================================
# RAIZ DO APP
# ==============================================================================
pasta_app <- obter_pasta_app()
pasta_raiz <- if (tolower(basename(pasta_app)) %in% c("extratorsia", "extradorsia")) {
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
    SELECT
        id_municipio_6 as id6,
        nome,
        sigla_uf
    FROM municipios
"))
municipios_df$id6 <- as.character(municipios_df$id6)

# Lookup rápido
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

# Dicionário SIGTAP (procedimentos)
proc_dict <- tryCatch({
  d <- as.data.frame(dbGetQuery(con_dict, "SELECT co_procedimento, no_procedimento FROM sigtap_procedimentos"))
  d$co_procedimento <- as.character(d$co_procedimento)
  distinct(d, co_procedimento, .keep_all = TRUE)
}, error = function(e) {
  message("SIGTAP não carregado: ", conditionMessage(e))
  data.frame(co_procedimento = character(0), no_procedimento = character(0), stringsAsFactors = FALSE)
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
# CONFIGURAÇÃO DOS PARQUETS (<pasta_app>/Tabelas_Todas/SIA/FINAL)
# ==============================================================================
PATH_DADOS <- file.path(pasta_app, "Tabelas_Todas")

listar_parquets_sia <- function() {
  pastas <- file.path(PATH_DADOS, c("SIA", "SIA-PA", "SIA_PA", "SIAPA"), "FINAL")
  pastas <- pastas[dir.exists(pastas)]
  if (length(pastas) == 0) return(character(0))

  arquivos <- unlist(lapply(pastas, function(p) {
    list.files(p, pattern = "^FINAL_SIA.*\\.parquet$", full.names = TRUE, ignore.case = TRUE)
  }))
  if (length(arquivos) == 0) return(character(0))
  normalizePath(arquivos, winslash = "/", mustWork = FALSE)
}

# Formato:
#   FINAL_SIA-PA_UF_CIDADE_ANO_MES.parquet  (ex.: FINAL_SIA-PA_PR_Irati_2024_01.parquet)
#   FINAL_SIA-BI_UF_CIDADE_ANO_MES.parquet
# Cidades com "_" também são tratadas.
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
  # Padrão mínimo: FINAL_SISTEMA_UF_CIDADE_ANO_MES  → n >= 6
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
  arquivos <- listar_parquets_sia()
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
  message("Parquets SIA encontrados: ", nrow(md))
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
# CARREGAR DADOS SIA COM DUCKDB (PARQUET)
# ==============================================================================
carregar_sia_parquet <- function(cidade_filtro = NULL, ano_filtro = NULL, mes_filtro = NULL) {

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
    select(caminho_n, CIDADE_ORIGEM = cidade, UF_ORIGEM = uf, SISTEMA = sistema)
  df$FILENAME <- normalizePath(df$FILENAME, winslash = "/", mustWork = FALSE)
  df <- df %>%
    left_join(md_map, by = c("FILENAME" = "caminho_n")) %>%
    select(-FILENAME)

  # Helper: primeira coluna existente
  pega_col <- function(opcoes) {
    achou <- intersect(names(df), opcoes)[1]
    if (is.na(achou)) rep(NA, nrow(df)) else df[[achou]]
  }

  # ---------------- Data / mês (PA_MVM AAAA-MM) ----------------
  mvm <- as.character(pega_col(c("PA_MVM", "MES_ANO")))
  mes <- ifelse(
    grepl("^\\d{4}-\\d{2}", mvm),
    suppressWarnings(as.numeric(substr(mvm, 6, 7))),
    suppressWarnings(as.numeric(substr(str_pad(mvm, 6, pad = "0"), 5, 6)))
  )
  df$MES <- ifelse(is.na(mes) | mes < 1 | mes > 12, 1, mes)
  df$MES_NOME <- factor(df$MES, levels = 1:12,
                        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                   "Jul", "Ago", "Set", "Out", "Nov", "Dez"))

  # ---------------- Procedimento ----------------
  df$PROC_COD <- as.character(pega_col(c("PA_PROC_ID", "PROCEDIMENTO", "COD_PROC")))
  df <- df %>%
    left_join(proc_dict, by = c("PROC_COD" = "co_procedimento")) %>%
    mutate(PROC_DESCRICAO = ifelse(is.na(no_procedimento), PROC_COD, no_procedimento)) %>%
    select(-no_procedimento)

  # Grupo = 2 primeiros dígitos
  df$GRUPO_PROC <- substr(df$PROC_COD, 1, 2)

  # ---------------- Quantidade ----------------
  qtd <- as.numeric(as.character(pega_col(c("PA_QTDPRO", "QUANTIDADE", "QTD"))))
  df$QUANTIDADE <- ifelse(is.na(qtd), 1, qtd)

  # ---------------- Produção / Consumo ----------------
  df$PROD_COD <- str_pad(as.character(pega_col(c("PA_UFMUN", "UFMUN", "MUNIC_PRES"))),
                         6, pad = "0")
  df$CONS_COD <- str_pad(as.character(pega_col(c("PA_MUNPCN", "MUNPCN", "MUNIC_RES"))),
                         6, pad = "0")

  df$CIDADE_PRODUCAO <- nome_municipio_vec(df$PROD_COD)
  df$CIDADE_CONSUMO  <- nome_municipio_vec(df$CONS_COD)

  # ---------------- Estabelecimento (CNES) ----------------
  cnes_raw <- as.character(pega_col(c("PA_CODUNI", "CNES", "CO_CNES")))
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

  # ---------------- Tipo de fluxo (cidade selecionada) ----------------
  codigo_ref <- codigo_municipio(cidade_filtro)
  if (!is.na(codigo_ref)) {
    cidade_ref <- cidade_filtro
    df$TIPO_FLUXO <- case_when(
      df$PROD_COD == codigo_ref & df$CONS_COD == codigo_ref ~ paste("Produção e Consumo em", cidade_ref),
      df$PROD_COD == codigo_ref & df$CONS_COD != codigo_ref ~ paste("Apenas Produção em", cidade_ref),
      df$PROD_COD != codigo_ref & df$CONS_COD == codigo_ref ~ paste("Apenas Consumo de", cidade_ref),
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

      selectInput("cidade_sia", tags$b(icon("city"), " Cidade:"),
                  choices = NULL, selected = NULL),

      selectInput("ano_sia", "Selecione o Ano:", choices = NULL),

      selectInput("mes_sia", "Selecione o Mês:",
                  choices = c("Todos", "Janeiro" = 1, "Fevereiro" = 2, "Março" = 3, "Abril" = 4,
                              "Maio" = 5, "Junho" = 6, "Julho" = 7, "Agosto" = 8,
                              "Setembro" = 9, "Outubro" = 10, "Novembro" = 11, "Dezembro" = 12),
                  selected = "Todos"),

      numericInput("pop_ref", "População de referência:", value = 60000, min = 1, step = 1000),
      hr(),
      menuItem("Visão Geral", tabName = "geral", icon = icon("tachometer-alt")),
      menuItem("Procedimentos", tabName = "procedimentos", icon = icon("stethoscope")),
      menuItem("Análise de Fluxo", tabName = "fluxo", icon = icon("exchange-alt")),
      menuItem("Estabelecimentos", tabName = "estabelecimentos", icon = icon("hospital")),
      menuItem("🗺️ Mapa da Produção", tabName = "mapa", icon = icon("map-marker-alt")),
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
          valueBoxOutput("total_procedimentos", width = 3),
          valueBoxOutput("total_estabelecimentos", width = 3),
          valueBoxOutput("procedimentos_por_hab", width = 3),
          valueBoxOutput("resolucao_local", width = 3)
        ),
        fluidRow(
          valueBoxOutput("producao_local", width = 3),
          valueBoxOutput("consumo_local", width = 3),
          valueBoxOutput("pacientes_fora", width = 3),
          valueBoxOutput("saldo_atendimento", width = 3)
        ),
        fluidRow(
          box(title = "Evolução Mensal dos Procedimentos", width = 12, status = "primary",
              plotlyOutput("evolucao_mensal", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Top 10 Procedimentos (SIGTAP traduzido)", width = 6, status = "success",
              plotlyOutput("top_procedimentos", height = "500px")
          ),
          box(title = "Top 10 Estabelecimentos", width = 6, status = "warning",
              plotlyOutput("top_estabelecimentos", height = "500px")
          )
        )
      ),

      # ========== PROCEDIMENTOS ==========
      tabItem(
        tabName = "procedimentos",
        fluidRow(
          box(title = "Distribuição por Grupo de Procedimentos", width = 12, status = "info",
              plotlyOutput("procedimentos_grupo", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Buscar Procedimento", width = 12, status = "warning",
              textInput("buscar_procedimento", "Digite o nome ou código do procedimento:",
                        placeholder = "Ex: consulta, pressão, 0301100039"),
              br(),
              DTOutput("tabela_proc_busca")
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
          box(title = "Fluxograma: Produção vs Consumo", width = 12, status = "primary",
              plotlyOutput("fluxo_sankey", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Municípios que mais utilizam serviços na cidade", width = 6, status = "warning",
              plotlyOutput("municipios_atendidos", height = "500px")
          ),
          box(title = "Destinos dos moradores (fora do município)", width = 6, status = "warning",
              plotlyOutput("destinos_moradores", height = "500px")
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
          box(title = "Produção por Estabelecimento (com nomes CNES)", width = 12, status = "danger",
              plotlyOutput("producao_estabelecimentos", height = "600px")
          )
        ),
        fluidRow(
          box(title = "Comparativo Mensal - Top 5", width = 12, status = "danger",
              plotlyOutput("comparativo_estabelecimentos", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Buscar Estabelecimento", width = 12, status = "info",
              textInput("buscar_estabelecimento", "Digite o nome do estabelecimento:",
                        placeholder = "Ex: Santa Casa, UBS, Centro"),
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
            title = "🗺️ Localização da Produção por Estabelecimento",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            div(
              style = "margin-bottom: 15px;",
              radioButtons("mapa_tipo_sia", "Tipo de Visualização:",
                           choices = c(
                             "📍 Pontos" = "pontos",
                             "🔥 Heatmap" = "heatmap",
                             "🔵 Cluster" = "cluster"
                           ),
                           inline = TRUE
              )),
            leafletOutput("mapa_sia", height = "600px")
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
          box(title = "Dados SIA com Tradução SIGTAP e Nomes CNES", width = 12, status = "primary",
              DTOutput("tabela_sia"),
              br(),
              downloadButton("download_sia", "📥 Baixar Dados (CSV)", class = "btn-success")
          )
        )
      ),

      # ========== RELATÓRIO ==========
      tabItem(
        tabName = "relatorio",
        fluidRow(
          box(title = "Relatório de Produção Ambulatorial", width = 12, status = "success",
              downloadButton("download_relatorio_sia", "📥 Baixar Relatório (HTML)", class = "btn-success"),
              br(), br(),
              htmlOutput("relatorio_sia")
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
      updateSelectInput(session, "cidade_sia", choices = opcoes, selected = "Todas")
    } else {
      updateSelectInput(session, "cidade_sia",
                        choices = c("Nenhum dado encontrado" = ""), selected = "")
      showNotification("Nenhum arquivo Parquet do SIA encontrado! Use o Extrator DATASUS primeiro.",
                       type = "error", duration = 10)
    }
  })

  # ========== CARREGAR ANOS DISPONÍVEIS ==========
  observeEvent(input$cidade_sia, {
    req(input$cidade_sia != "", input$cidade_sia != "Nenhum dado encontrado")

    cidade_param <- if (input$cidade_sia == "Todas") NULL else input$cidade_sia
    anos <- obter_anos_disponiveis(cidade_param)

    if (length(anos) > 0) {
      updateSelectInput(session, "ano_sia", choices = anos, selected = anos[1])
    } else {
      updateSelectInput(session, "ano_sia",
                        choices = c("Nenhum ano encontrado" = ""), selected = "")
    }
  })

  # ========== TÍTULO ==========
  output$titulo_dashboard <- renderUI({
    if (is.null(input$cidade_sia) || input$cidade_sia == "" || input$cidade_sia == "Todas") {
      paste("SIA - Sistema de Informação Ambulatorial - Todas as Cidades")
    } else {
      paste("SIA - Sistema de Informação Ambulatorial -", input$cidade_sia)
    }
  })

  # ========== CIDADE REFERÊNCIA ==========
  cidade_ref <- reactive({
    if (is.null(input$cidade_sia) || input$cidade_sia %in% c("", "Todas")) NULL else input$cidade_sia
  })

  codigo_ref <- reactive({
    codigo_municipio(cidade_ref())
  })

  # ========== CARREGAR DADOS ==========
  sia_data <- reactive({
    req(input$ano_sia, input$cidade_sia,
        input$cidade_sia != "", input$cidade_sia != "Nenhum dado encontrado")
    req(input$ano_sia != "Nenhum ano encontrado")

    cidade_param <- if (input$cidade_sia == "Todas") NULL else input$cidade_sia

    df <- carregar_sia_parquet(
      cidade_filtro = cidade_param,
      ano_filtro = as.numeric(input$ano_sia),
      mes_filtro = input$mes_sia
    )

    if (is.null(df) || nrow(df) == 0) {
      showNotification("Nenhum dado encontrado para os filtros selecionados.", type = "warning")
      return(NULL)
    }

    df
  })

  # ========== DADOS MAPA ==========
  dados_mapa <- reactive({
    req(sia_data())

    df <- sia_data() %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      group_by(CNES, CNES_NOME, latitude, longitude) %>%
      summarise(
        total_procedimentos = sum(QUANTIDADE, na.rm = TRUE),
        procedimentos_distintos = n_distinct(PROC_COD, na.rm = TRUE),
        pacientes_unicos = n_distinct(PROD_COD, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        popup_text = paste0(
          "<b>", CNES_NOME, "</b><br>",
          "🩺 Procedimentos: ", format(total_procedimentos, big.mark = "."), "<br>",
          "📋 Tipos distintos: ", procedimentos_distintos, "<br>",
          "👥 Municípios produtores: ", pacientes_unicos
        )
      )

    df
  })

  # ========== VALUE BOXES ==========
  output$total_procedimentos <- renderValueBox({
    req(sia_data())
    cidade_nome <- if (is.null(cidade_ref())) "Todas as Cidades" else cidade_ref()
    valueBox(
      value = format(sum(sia_data()$QUANTIDADE, na.rm = TRUE), big.mark = "."),
      subtitle = paste("Procedimentos -", cidade_nome),
      icon = icon("stethoscope"),
      color = "blue"
    )
  })

  output$total_estabelecimentos <- renderValueBox({
    req(sia_data())
    valueBox(
      value = n_distinct(sia_data()$CNES, na.rm = TRUE),
      subtitle = "Estabelecimentos",
      icon = icon("hospital"),
      color = "green"
    )
  })

  output$procedimentos_por_hab <- renderValueBox({
    req(sia_data(), populacao())
    perc <- round(sum(sia_data()$QUANTIDADE, na.rm = TRUE) / populacao(), 2)
    valueBox(
      value = paste(perc, "/hab"),
      subtitle = "Procedimentos por Habitante",
      icon = icon("users"),
      color = "light-blue"
    )
  })

  output$resolucao_local <- renderValueBox({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      valueBox(value = "—", subtitle = "Resolução Local (selecione cidade)",
               icon = icon("check-circle"), color = "yellow")
    } else {
      resolvido <- sum(sia_data()$QUANTIDADE[
        sia_data()$PROD_COD == cod & sia_data()$CONS_COD == cod], na.rm = TRUE)
      total_moradores <- sum(sia_data()$QUANTIDADE[
        sia_data()$CONS_COD == cod], na.rm = TRUE)
      perc <- ifelse(total_moradores > 0, round(resolvido / total_moradores * 100, 1), 0)
      valueBox(
        value = paste(perc, "%"),
        subtitle = "Resolução Local (atendido no município)",
        icon = icon("check-circle"),
        color = "green"
      )
    }
  })

  output$producao_local <- renderValueBox({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      valueBox(value = "—", subtitle = "Produção local", icon = icon("industry"), color = "yellow")
    } else {
      producao <- sum(sia_data()$QUANTIDADE[sia_data()$PROD_COD == cod], na.rm = TRUE)
      perc <- round(producao / sum(sia_data()$QUANTIDADE, na.rm = TRUE) * 100, 1)
      valueBox(
        value = paste(format(producao, big.mark = "."), "(", perc, "%)"),
        subtitle = "Produção local",
        icon = icon("industry"),
        color = "yellow"
      )
    }
  })

  output$consumo_local <- renderValueBox({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      valueBox(value = "—", subtitle = "Consumo por moradores", icon = icon("shopping-cart"), color = "fuchsia")
    } else {
      consumo <- sum(sia_data()$QUANTIDADE[sia_data()$CONS_COD == cod], na.rm = TRUE)
      perc <- round(consumo / sum(sia_data()$QUANTIDADE, na.rm = TRUE) * 100, 1)
      valueBox(
        value = paste(format(consumo, big.mark = "."), "(", perc, "%)"),
        subtitle = "Consumo por moradores",
        icon = icon("shopping-cart"),
        color = "fuchsia"
      )
    }
  })

  output$pacientes_fora <- renderValueBox({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      valueBox(value = "—", subtitle = "Atendidos de fora", icon = icon("users"), color = "orange")
    } else {
      fora <- sum(sia_data()$QUANTIDADE[
        sia_data()$PROD_COD == cod & sia_data()$CONS_COD != cod], na.rm = TRUE)
      valueBox(
        value = format(fora, big.mark = "."),
        subtitle = "Atendidos de fora",
        icon = icon("users"),
        color = "orange"
      )
    }
  })

  output$saldo_atendimento <- renderValueBox({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      valueBox(value = "—", subtitle = "Saldo", icon = icon("balance-scale"), color = "yellow")
    } else {
      atendidos_fora <- sum(sia_data()$QUANTIDADE[
        sia_data()$PROD_COD == cod & sia_data()$CONS_COD != cod], na.rm = TRUE)
      saida_moradores <- sum(sia_data()$QUANTIDADE[
        sia_data()$PROD_COD != cod & sia_data()$CONS_COD == cod], na.rm = TRUE)
      saldo <- atendidos_fora - saida_moradores
      color <- ifelse(saldo > 0, "green", ifelse(saldo < 0, "red", "yellow"))
      valueBox(
        value = format(abs(saldo), big.mark = "."),
        subtitle = ifelse(saldo > 0, "Superávit de atendimentos", "Déficit de atendimentos"),
        icon = icon("balance-scale"),
        color = color
      )
    }
  })

  # ========== GRÁFICOS ==========
  output$evolucao_mensal <- renderPlotly({
    req(sia_data())
    df <- sia_data() %>%
      group_by(MES, MES_NOME) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 2),
            marker = list(color = "#2980b9", size = 8)) %>%
      layout(title = paste("Procedimentos por Mês -", input$ano_sia),
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Quantidade de Procedimentos"))
  })

  output$top_procedimentos <- renderPlotly({
    req(sia_data())
    df <- sia_data() %>%
      group_by(PROC_COD, PROC_DESCRICAO) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    plot_ly(df, x = ~Total, y = ~reorder(PROC_DESCRICAO, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#2ecc71")) %>%
      layout(title = "Top 10 Procedimentos",
             xaxis = list(title = "Quantidade"),
             yaxis = list(title = ""))
  })

  output$top_estabelecimentos <- renderPlotly({
    req(sia_data())
    df <- sia_data() %>%
      group_by(CNES, CNES_NOME) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    plot_ly(df, x = ~Total, y = ~reorder(CNES_NOME, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = "Top 10 Estabelecimentos",
             xaxis = list(title = "Procedimentos"),
             yaxis = list(title = ""))
  })

  # ========== PROCEDIMENTOS ==========
  output$procedimentos_grupo <- renderPlotly({
    req(sia_data())
    df <- sia_data() %>%
      group_by(GRUPO_PROC) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)

    plot_ly(df, x = ~GRUPO_PROC, y = ~Total, type = "bar",
            marker = list(color = "#9b59b6")) %>%
      layout(title = "Procedimentos por Grupo",
             xaxis = list(title = "Grupo"),
             yaxis = list(title = "Quantidade"))
  })

  output$tabela_proc_busca <- renderDT({
    req(sia_data(), input$buscar_procedimento)

    if (input$buscar_procedimento != "") {
      busca <- toupper(input$buscar_procedimento)
      df <- sia_data() %>%
        filter(grepl(busca, PROC_COD, ignore.case = TRUE) |
               grepl(busca, PROC_DESCRICAO, ignore.case = TRUE)) %>%
        group_by(PROC_COD, PROC_DESCRICAO) %>%
        summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Total))

      datatable(df,
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_procedimento))
    } else {
      datatable(data.frame(Mensagem = "Digite um nome ou código para buscar"),
                rownames = FALSE,
                options = list(dom = 't'))
    }
  })

  # ========== FLUXO ==========
  output$resumo_fluxo <- renderUI({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      return(div(style = "padding: 20px;",
                 h3("Selecione uma cidade para ver o fluxo de pacientes.")))
    }

    cidade <- cidade_ref()
    producao <- sum(sia_data()$QUANTIDADE[sia_data()$PROD_COD == cod], na.rm = TRUE)
    consumo <- sum(sia_data()$QUANTIDADE[sia_data()$CONS_COD == cod], na.rm = TRUE)
    ambos <- sum(sia_data()$QUANTIDADE[
      sia_data()$PROD_COD == cod & sia_data()$CONS_COD == cod], na.rm = TRUE)
    apenas_prod <- producao - ambos
    apenas_cons <- consumo - ambos
    saldo <- apenas_prod - apenas_cons

    div(
      style = "display: flex; justify-content: space-around; text-align: center; flex-wrap: wrap;",
      div(style = "background: #2ecc71; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(producao, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Produção em", cidade)),
          p(style = "margin: 0; color: white; font-size: 12px;", "Serviços prestados")
      ),
      div(style = "background: #3498db; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(consumo, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Consumo por", cidade)),
          p(style = "margin: 0; color: white; font-size: 12px;", "Serviços utilizados")
      ),
      div(style = "background: #f39c12; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(apenas_prod, big.mark = ".")),
          p(style = "margin: 0; color: white;", "Atendidos de fora"),
          p(style = "margin: 0; color: white; font-size: 12px;", "Moradores de outros municípios")
      ),
      div(style = "background: #e74c3c; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(apenas_cons, big.mark = ".")),
          p(style = "margin: 0; color: white;", "Moradores fora"),
          p(style = "margin: 0; color: white; font-size: 12px;", "Buscaram serviços em outros municípios")
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
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) {
      return(plot_ly() %>% layout(title = "Selecione uma cidade"))
    }

    cidade <- cidade_ref()
    prod_local <- sia_data()$PROD_COD == cod
    cons_local <- sia_data()$CONS_COD == cod
    qtd <- sia_data()$QUANTIDADE

    valores <- c(
      sum(qtd[prod_local & cons_local], na.rm = TRUE),
      sum(qtd[prod_local & !cons_local], na.rm = TRUE),
      sum(qtd[!prod_local & cons_local], na.rm = TRUE),
      sum(qtd[!prod_local & !cons_local], na.rm = TRUE)
    )

    plot_ly(type = "sankey", orientation = "h",
            node = list(
              label = c(paste("Produção em", cidade), "Produção fora",
                        paste("Consumo em", cidade), "Consumo fora"),
              color = c("#2ecc71", "#e74c3c", "#3498db", "#f39c12"),
              pad = 15, thickness = 20
            ),
            link = list(source = c(0, 0, 1, 1), target = c(2, 3, 2, 3), value = valores)) %>%
      layout(title = paste("Fluxo de Atendimento -", cidade),
             font = list(size = 12))
  })

  output$municipios_atendidos <- renderPlotly({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) return(plot_ly() %>% layout(title = "Selecione uma cidade"))

    cidade <- cidade_ref()
    df <- sia_data() %>%
      filter(PROD_COD == cod & CONS_COD != cod & !is.na(CONS_COD)) %>%
      group_by(CONS_COD, CIDADE_CONSUMO) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = paste("Nenhum paciente de fora atendido em", cidade)))
    }

    plot_ly(df, x = ~Total, y = ~reorder(CIDADE_CONSUMO, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#f39c12")) %>%
      layout(title = paste("Municípios que mais utilizam serviços em", cidade),
             xaxis = list(title = "Procedimentos"),
             yaxis = list(title = ""))
  })

  output$destinos_moradores <- renderPlotly({
    req(sia_data(), codigo_ref())
    cod <- codigo_ref()
    if (is.na(cod)) return(plot_ly() %>% layout(title = "Selecione uma cidade"))

    cidade <- cidade_ref()
    df <- sia_data() %>%
      filter(PROD_COD != cod & CONS_COD == cod & !is.na(PROD_COD)) %>%
      group_by(PROD_COD, CIDADE_PRODUCAO) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(10)

    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = paste("Nenhum morador de", cidade, "buscou serviços fora")))
    }

    plot_ly(df, x = ~Total, y = ~reorder(CIDADE_PRODUCAO, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#e74c3c")) %>%
      layout(title = paste("Destinos dos moradores de", cidade),
             xaxis = list(title = "Procedimentos"),
             yaxis = list(title = ""))
  })

  output$evolucao_fluxo <- renderPlotly({
    req(sia_data())
    df <- sia_data() %>%
      group_by(MES_NOME, TIPO_FLUXO) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop')

    plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~TIPO_FLUXO,
            type = "scatter", mode = "lines+markers") %>%
      layout(title = "Evolução Mensal do Fluxo de Atendimento",
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Procedimentos"))
  })

  # ========== ESTABELECIMENTOS ==========
  output$producao_estabelecimentos <- renderPlotly({
    req(sia_data())
    df <- sia_data() %>%
      filter(!is.na(CNES_NOME)) %>%
      group_by(CNES, CNES_NOME) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)

    plot_ly(df, x = ~Total, y = ~reorder(CNES_NOME, Total),
            type = "bar", orientation = "h",
            marker = list(color = "#f39c12")) %>%
      layout(title = "Produção por Estabelecimento",
             xaxis = list(title = "Procedimentos"),
             yaxis = list(title = ""))
  })

  output$comparativo_estabelecimentos <- renderPlotly({
    req(sia_data())
    top5 <- sia_data() %>%
      filter(!is.na(CNES)) %>%
      group_by(CNES) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      slice_max(Total, n = 5) %>%
      pull(CNES)

    if (length(top5) > 0) {
      df <- sia_data() %>%
        filter(CNES %in% top5) %>%
        group_by(CNES_NOME, MES_NOME) %>%
        summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop')

      plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~CNES_NOME,
              type = "scatter", mode = "lines+markers") %>%
        layout(title = "Comparativo Mensal - Top 5 Estabelecimentos",
               xaxis = list(title = "Mês"),
               yaxis = list(title = "Procedimentos"))
    } else {
      plot_ly() %>% layout(title = "Sem dados suficientes para comparativo")
    }
  })

  output$tabela_estab_busca <- renderDT({
    req(sia_data())

    if (!is.null(input$buscar_estabelecimento) && input$buscar_estabelecimento != "") {
      busca <- toupper(input$buscar_estabelecimento)
      df <- sia_data() %>%
        filter(grepl(busca, toupper(CNES_NOME), ignore.case = TRUE)) %>%
        group_by(CNES, CNES_NOME) %>%
        summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(Total))

      datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_estabelecimento))
    } else {
      datatable(data.frame(Mensagem = "Digite um nome para buscar"),
                rownames = FALSE, options = list(dom = 't'))
    }
  })

  # ========== MAPA ==========
  output$mapa_sia <- renderLeaflet({
    req(dados_mapa())

    df <- dados_mapa()

    if (nrow(df) == 0) {
      return(leaflet() %>%
               addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = -51.9, lat = -24.6, zoom = 6))
    }

    pal <- colorNumeric(palette = "RdYlBu", domain = df$total_procedimentos, reverse = TRUE)

    map <- leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(
        lng1 = min(df$longitude), lat1 = min(df$latitude),
        lng2 = max(df$longitude), lat2 = max(df$latitude)
      )

    if (input$mapa_tipo_sia == "pontos") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          radius = ~sqrt(total_procedimentos) * 2,
          color = ~pal(total_procedimentos), fillOpacity = 0.7,
          stroke = TRUE, weight = 2,
          popup = ~popup_text, label = ~CNES_NOME,
          group = "Estabelecimentos"
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~total_procedimentos,
                  title = "Total de Procedimentos", opacity = 0.7)

    } else if (input$mapa_tipo_sia == "heatmap") {
      map <- map %>%
        addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~total_procedimentos,
                   blur = 20, max = 0.05, radius = 15, group = "Heatmap") %>%
        addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 6,
                         color = "black", fillOpacity = 0.3, stroke = TRUE, weight = 1,
                         popup = ~popup_text, label = ~CNES_NOME, group = "Marcadores")

    } else if (input$mapa_tipo_sia == "cluster") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude, lat = ~latitude, radius = 8,
          color = "#3498db", fillOpacity = 0.6,
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
      select(CNES_NOME, total_procedimentos, procedimentos_distintos, pacientes_unicos) %>%
      arrange(desc(total_procedimentos))

    datatable(df,
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
              ),
              colnames = c("Estabelecimento", "Total Procedimentos", "Tipos Distintos", "Municípios Atendidos"),
              rownames = FALSE)
  })

  # ========== DADOS BRUTOS ==========
  output$tabela_sia <- renderDT({
    req(sia_data())

    tabela <- sia_data() %>%
      select(CNES, CNES_NOME, PROC_COD, PROC_DESCRICAO, QUANTIDADE,
             MES_NOME, CIDADE_PRODUCAO, CIDADE_CONSUMO, TIPO_FLUXO) %>%
      head(500)

    datatable(as.data.frame(tabela),
              options = list(scrollX = TRUE, pageLength = 20),
              rownames = FALSE,
              colnames = c("CNES", "Estabelecimento", "Código", "Procedimento",
                           "Qtd", "Mês", "Município Produção", "Município Consumo", "Fluxo"))
  })

  output$download_sia <- downloadHandler(
    filename = function() paste0("sia_", input$cidade_sia, "_", input$ano_sia, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(sia_data())
      write.csv(sia_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ========== RELATÓRIO ==========
  output$relatorio_sia <- renderUI({
    req(sia_data())

    cidade_nome <- if (is.null(cidade_ref())) "Todas as Cidades" else cidade_ref()
    total <- sum(sia_data()$QUANTIDADE, na.rm = TRUE)
    cod <- codigo_ref()

    if (!is.na(cod)) {
      producao <- sum(sia_data()$QUANTIDADE[sia_data()$PROD_COD == cod], na.rm = TRUE)
      consumo <- sum(sia_data()$QUANTIDADE[sia_data()$CONS_COD == cod], na.rm = TRUE)
      taxa_resolucao <- ifelse(consumo > 0,
                               round(sum(sia_data()$QUANTIDADE[
                                 sia_data()$PROD_COD == cod & sia_data()$CONS_COD == cod],
                                 na.rm = TRUE) / consumo * 100, 1),
                               0)
    } else {
      producao <- NA; consumo <- NA; taxa_resolucao <- NA
    }

    top_proc <- sia_data() %>%
      group_by(PROC_DESCRICAO) %>%
      summarise(Total = sum(QUANTIDADE, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(5)

    proc_text <- paste(paste0(top_proc$PROC_DESCRICAO, " (",
                              format(top_proc$Total, big.mark = "."), ")"),
                       collapse = "<br>")

    mes_txt <- if (input$mes_sia != "Todos") paste0(" | Mês: ", input$mes_sia) else ""

    html_resumo <- paste0(
      "<ul>",
      "<li>Total de Procedimentos: <strong>", format(total, big.mark = "."), "</strong></li>",
      if (!is.na(producao)) paste0("<li>Produção local: <strong>", format(producao, big.mark = "."), "</strong></li>") else "",
      if (!is.na(consumo)) paste0("<li>Consumo por moradores: <strong>", format(consumo, big.mark = "."), "</strong></li>") else "",
      if (!is.na(taxa_resolucao)) paste0("<li>Taxa de Resolução Local: <strong>", taxa_resolucao, "%</strong></li>") else "",
      "</ul>"
    )

    HTML(paste0("
    <div style='font-family: Arial; padding: 20px;'>
      <h1 style='color: #3498db;'>RELATÓRIO DE PRODUÇÃO AMBULATORIAL</h1>
      <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sia, mes_txt, "</h3>
      <hr/>

      <h2>1. METODOLOGIA</h2>
      <p>Dados do SIA (Sistema de Informação Ambulatorial) via DATASUS, lidos de arquivos Parquet gerados pelo Extrator DATASUS.</p>
      <p>Procedimentos traduzidos usando dicionário SIGTAP.</p>
      <p>Estabelecimentos identificados via CNES.</p>

      <h2>2. NÚMEROS GLOBAIS</h2>
      ", html_resumo, "

      <h2>3. PRINCIPAIS PROCEDIMENTOS</h2>
      <p>", proc_text, "</p>

      <hr/>
      <p>Relatório gerado em ", Sys.Date(), "</p>
    </div>
    "))
  })

  output$download_relatorio_sia <- downloadHandler(
    filename = function() paste0("relatorio_sia_", input$cidade_sia, "_", input$ano_sia, "_", Sys.Date(), ".html"),
    content = function(file) {
      req(sia_data())
      cidade_nome <- if (is.null(cidade_ref())) "Todas" else cidade_ref()
      total <- sum(sia_data()$QUANTIDADE, na.rm = TRUE)

      html <- paste0("
      <html>
      <head><meta charset='UTF-8'><style>body{font-family:Arial;margin:40px;}</style></head>
      <body>
        <h1>RELATÓRIO DE PRODUÇÃO AMBULATORIAL</h1>
        <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sia, "</h3>
        <hr/>
        <h2>Resumo</h2>
        <ul>
          <li>Total de Procedimentos: ", format(total, big.mark = "."), "</li>
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