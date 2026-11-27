# ==============================================================================
# PORTAL IBGE / IRATI ANALYTICS 2026 - MENU PRINCIPAL
# COM ANALISADOR INTEGRADO + MANUAL EMBUTIDO + AVISOS
# ==============================================================================
# COMO USAR:
#   1. Tenha o R instalado (https://cran.r-project.org/)
#   2. Abra este arquivo no RStudio (ou R puro)
#   3. Clique em "Run App" (RStudio) OU execute:
#        source("app.R")
#   4. O navegador abrirá automaticamente.
#   5. Na primeira execução, os pacotes faltantes serão instalados
#      automaticamente (pode demorar alguns minutos).
# ==============================================================================

# 0. AVISO INICIAL NO CONSOLE ------------------------------------------------
cat("\n")
cat("======================================================================\n")
cat("   IRATI ANALYTICS 2026 - INICIANDO\n")
cat("======================================================================\n")
cat("Verificando pacotes necessários...\n")

# 1. INSTALAÇÃO AUTOMÁTICA DE PACOTES ---------------------------------------
pacotes_necessarios <- c(
  "shiny", "shinydashboard", "shinyWidgets", "leaflet",
  "sf", "sfarrow", "dplyr", "bslib", "arrow", "tidyr",
  "plotly", "stringr", "geobr", "mapview", "censobr",
  "DT", "writexl", "readxl", "zip", "ggplot2", "bs4Dash"
)

faltantes <- pacotes_necessarios[
  !sapply(pacotes_necessarios, requireNamespace, quietly = TRUE)
]

if (length(faltantes) > 0) {
  cat("\n⚠  Pacotes faltantes detectados:\n")
  cat(paste0("   - ", faltantes, collapse = "\n"), "\n")
  cat("\nInstalando automaticamente (pode levar vários minutos)...\n")
  cat("Isso só acontece na PRIMEIRA execução.\n\n")

  # Instala silenciosamente; aborta com mensagem clara se falhar
  install_ok <- tryCatch({
    install.packages(faltantes, dependencies = TRUE)
    TRUE
  }, error = function(e) {
    cat("\n❌ ERRO ao instalar pacotes:\n", conditionMessage(e), "\n")
    cat("Verifique sua conexão com a internet e tente novamente.\n")
    FALSE
  })

  if (!install_ok) {
    stop("Não foi possível instalar os pacotes necessários. Abortando.")
  }

  # Revalida
  ainda_faltam <- faltantes[
    !sapply(faltantes, requireNamespace, quietly = TRUE)
  ]
  if (length(ainda_faltam) > 0) {
    stop(paste(
      "❌ Ainda faltam pacotes após a instalação:\n",
      paste(ainda_faltam, collapse = ", "),
      "\nVerifique os erros acima e instale manualmente com install.packages()."
    ))
  }
  cat("\n✅ Todos os pacotes foram instalados com sucesso!\n")
} else {
  cat("✅ Todos os pacotes já estão instalados.\n")
}

# Carrega as bibliotecas
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(leaflet)
  library(sf)
  library(sfarrow)
  library(dplyr)
  library(bslib)
  library(arrow)
  library(tidyr)
  library(plotly)
  library(stringr)
  library(geobr)
  library(mapview)
  library(censobr)
  library(DT)
  library(writexl)
  library(readxl)
  library(zip)
  library(ggplot2)
  library(bs4Dash)
})

# 2. CONFIGURAÇÃO DE AMBIENTE (SEM setwd HARDCODED) -------------------------
# Usa a pasta onde ESTE app.R está salvo como raiz do projeto
pasta_projeto <- tryCatch({
  # Em shiny::runApp, o working dir já é a pasta do app.R
  if (file.exists("app.R")) getwd() else getwd()
}, error = function(e) getwd())

pasta_dados <- file.path(pasta_projeto, "DADOS")
if (!dir.exists(pasta_dados)) {
  dir.create(pasta_dados, recursive = TRUE, showWarnings = FALSE)
  cat("📁 Pasta DADOS/ criada em:", pasta_dados, "\n")
}

# Mensagem de status
cat("\n📂 Pasta do projeto:", pasta_projeto, "\n")
cat("📂 Pasta de dados :", pasta_dados, "\n")
cat("\n🚀 Iniciando servidor Shiny...\n")
cat("   O navegador abrirá automaticamente.\n")
cat("   Para encerrar: feche esta janela do R ou pressione ESC.\n")
cat("======================================================================\n\n")

# 3. MÓDULO MANUAL (NOVO) ---------------------------------------------------
ui_manual <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .manual-box {
        background: #f8f9fa;
        border-left: 5px solid #0d6efd;
        padding: 18px 22px;
        margin-bottom: 18px;
        border-radius: 10px;
        line-height: 1.7;
      }
      .manual-box h4 { color: #0d6efd; margin-top: 0; }
      .manual-step {
        background: #fff;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 12px 16px;
        margin-bottom: 10px;
      }
      .manual-code {
        background: #2c3e50;
        color: #ecf0f1;
        padding: 10px 14px;
        border-radius: 6px;
        font-family: monospace;
        font-size: 0.9em;
        margin: 8px 0;
      }
      .manual-alert {
        background: #fff3cd;
        border-left: 5px solid #ffc107;
        padding: 12px 16px;
        border-radius: 8px;
        margin-bottom: 14px;
      }
      .manual-ok {
        background: #d1e7dd;
        border-left: 5px solid #198754;
        padding: 12px 16px;
        border-radius: 8px;
        margin-bottom: 14px;
      }
    "))),

    titlePanel(h2(icon("book-open"), " Manual de Uso - Irati Analytics 2026",
                  style = "color: #2c3e50;")),

    br(),

    div(class = "manual-box",
      h4(icon("rocket"), " Início rápido"),
      p("Ao abrir o app, o navegador exibirá esta interface. Use o menu lateral esquerdo para navegar entre as seções."),
      tags$ol(
        tags$li(strong("Censo IBGE:"), " baixe e visualize tabelas do Censo 2022/2010 por município."),
        tags$li(strong("Mapas:"), " carregue ou baixe camadas geoespaciais (setores, bairros, distritos, contorno)."),
        tags$li(strong("Análise Espacial:"), " cruze dados do CENSO com MAPAS salvos."),
        tags$li(strong("Manual:"), " esta página de ajuda.")
      )
    ),

    div(class = "manual-alert",
      icon("triangle-exclamation"), strong(" Primeira execução:"),
      " a instalação automática dos pacotes pode levar de 5 a 20 minutos, dependendo da sua conexão. Isso ocorre apenas uma vez."
    ),

    div(class = "manual-box",
      h4(icon("folder-tree"), " Estrutura de pastas"),
      p("O app organiza os dados automaticamente na pasta ", code("DADOS/"), ":"),
      div(class = "manual-code",
        "DADOS/", br(),
        "  └─ i RATI_PR/", br(),
        "      └─ ANO_2022/", br(),
        "          ├─ CENSO/   → tabelas do censo (.parquet)", br(),
        "          └─ MAPAS/   → camadas geoespaciais (.parquet)"
      ),
      p("Você pode copiar essa pasta para outro computador para reaproveitar os dados já baixados.")
    ),

    div(class = "manual-box",
      h4(icon("users"), " Seção 1 - Censo IBGE"),
      div(class = "manual-step",
        tags$b("Passo 1."), " Escolha o ano (2022 ou 2010), a tabela (Basico, Domicilio, etc.), a UF e o município.",
        br(), tags$b("Passo 2."), " Clique em ", span(style = "color:#0d6efd;font-weight:bold;", "Carregar Dados."),
        br(), tags$b("Passo 3."), " Visualize na aba ", em("Tabela"), " ou consulte os campos na aba ", em("Dicionário."),
        br(), tags$b("Passo 4."), " Clique em ", span(style = "color:#198754;font-weight:bold;", "Salvar na Pasta DADOS"),
        " para guardar localmente ou em ", em("Exportar Parquet"), " para baixar o arquivo."
      )
    ),

    div(class = "manual-box",
      h4(icon("map-location-dot"), " Seção 2 - Mapas"),
      div(class = "manual-step",
        tags$b("Passo 1."), " Digite o nome ou código do município (ex.: Irati ou 4110706).",
        br(), tags$b("Passo 2."), " Escolha a camada e o ano.",
        br(), tags$b("Passo 3."), " Use ", span(style = "color:#ffc107;font-weight:bold;", "Carregar da Estrutura DADOS"),
        " se já tiver baixado antes, ou ", span(style = "color:#0d6efd;font-weight:bold;", "Buscar no IBGE"), " para baixar agora.",
        br(), tags$b("Passo 4."), " Clique em ", span(style = "color:#198754;font-weight:bold;", "Salvar na Estrutura DADOS"), " para guardar."
      )
    ),

    div(class = "manual-box",
      h4(icon("chart-line"), " Seção 3 - Análise Espacial"),
      div(class = "manual-step",
        tags$b("Passo 1."), " Selecione uma cidade que já tenha dados salvos em DADOS/.",
        br(), tags$b("Passo 2."), " Escolha a variável do CENSO e a camada do MAPA.",
        br(), tags$b("Passo 3."), " Clique em ", span(style = "color:#198754;font-weight:bold;", "Executar Análise"), ".",
        br(), tags$b("Passo 4."), " Navegue pelas abas (Visão Geral, Mapa, CENSO, Dados).",
        br(), tags$b("Passo 5."), " Exporte um relatório HTML em ", em("Exportar Relatório"), "."
      )
    ),

    div(class = "manual-ok",
      icon("circle-check"), strong(" Dica:"), " se algo travar, feche o R, delete a pasta ",
      code("DADOS/"), " (se quiser recomeçar) e execute novamente. Os dados serão baixados de novo."
    ),

    div(class = "manual-box",
      h4(icon("bug"), " Solução de problemas"),
      tags$ul(
        tags$li(strong("“Pacote não encontrado”:"), " rode manualmente no console: ",
                code('install.packages(c("shiny","sf","arrow"))'), " e reabra o app."),
        tags$li(strong("Navegador não abre:"), " copie o endereço mostrado no console (ex.: ",
                code("http://127.0.0.1:XXXX"), ") e cole no navegador."),
        tags$li(strong("Erro ao baixar do IBGE:"), " verifique a conexão com a internet; o IBGE pode estar instável."),
        tags$li(strong("App lento:"), " evite carregar tabelas grandes no plano gratuito; prefira anos/camadas menores.")
      )
    ),

    div(class = "manual-box",
      h4(icon("info-circle"), " Sobre"),
      p("Irati Analytics 2026 - versão standalone. Desenvolvido em R/Shiny."),
      p("Fontes dos dados: IBGE (Censo 2022/2010) via pacotes ",
        code("censobr"), " e ", code("geobr"), ".")
    )
  )
}

# 4. MÓDULO CENSO ------------------------------------------------------------
ui_censo <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .info-box-censo {
        background-color: #e7f3ff;
        border-left: 4px solid #0d6efd;
        padding: 10px;
        margin-top: 10px;
        border-radius: 8px;
      }
    "))),

    titlePanel(h2("Explorador Universal IBGE - Censo 2022", style = "color: #2c3e50;")),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        pickerInput("ano_censo", "Ano:", choices = c("2022", "2010"), selected = "2022"),
        selectInput("dataset_censo", "Tabela:",
                    choices = c("Basico", "Domicilio", "Pessoas", "ResponsavelRenda",
                                "Indigenas", "Quilombolas", "Entorno", "Obitos", "Preliminares")),
        pickerInput("estado_censo", "UF:",
                    choices = c("PR", "SP", "RJ", "MG", "SC", "RS", "BA", "CE", "PE", "AM"),
                    selected = "PR", options = list(`live-search` = TRUE)),
        uiOutput("ui_municipio_censo"),
        hr(),
        actionButton("btn_load_censo", "Carregar Dados", icon = icon("sync"),
                    class = "btn-primary", style = "width: 100%;"),
        br(), br(),
        actionButton("btn_save_censo", "Salvar na Pasta DADOS", icon = icon("save"),
                    class = "btn-success", style = "width: 100%;"),
        br(), br(),
        downloadButton("btn_export_censo", "Exportar Parquet", class = "btn-info", style = "width: 100%"),
        hr(),
        div(class = "info-box-censo",
          icon("folder-tree"), tags$strong("Estrutura:"),
          tags$p("DADOS/CIDADE_UF/ANO_2022/CENSO/")
        )
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Tabela", icon = icon("table"), br(), DTOutput("tabela_censo")),
          tabPanel("Dicionário", icon = icon("book"), br(), DTOutput("dicionario_censo"))
        )
      )
    )
  )
}

server_censo <- function(input, output, session) {

  valores_censo <- reactiveValues(dados = NULL, municipio_nome = NULL, uf = NULL)

  limpar_nome <- function(nome) {
    nome_limpo <- iconv(nome, to = "ASCII//TRANSLIT")
    nome_limpo <- gsub("[^a-zA-Z0-9]", "_", nome_limpo)
    nome_limpo <- gsub("_+", "_", nome_limpo)
    nome_limpo <- tolower(nome_limpo)
    return(nome_limpo)
  }

  lista_munis <- reactive({
    req(input$estado_censo)
    cods <- c("PR" = 41, "SP" = 35, "RJ" = 33, "MG" = 31, "SC" = 42, "RS" = 43,
              "BA" = 29, "CE" = 23, "PE" = 26, "AM" = 13)

    withProgress(message = "Buscando municípios...", {
      tryCatch({
        q <- censobr::read_tracts(dataset = "Basico",
                                  year = as.numeric(input$ano_censo),
                                  as_data_frame = FALSE)
        q %>%
          filter(code_state == cods[input$estado_censo]) %>%
          select(name_muni) %>%
          distinct() %>%
          collect() %>%
          pull(name_muni) %>%
          sort()
      }, error = function(e) {
        showNotification(paste("Erro ao buscar municípios:", e$message),
                         type = "error", duration = 8)
        return(c("Irati"))  # fallback
      })
    })
  })

  output$ui_municipio_censo <- renderUI({
    pickerInput("municipio_censo", "Município:", choices = lista_munis(),
                selected = "Irati", options = list(`live-search` = TRUE))
  })

  dados_final <- eventReactive(input$btn_load_censo, {
    req(input$dataset_censo, input$municipio_censo)

    withProgress(message = "Baixando dados...", {
      tryCatch({
        df <- censobr::read_tracts(dataset = input$dataset_censo,
                                   year = as.numeric(input$ano_censo),
                                   as_data_frame = FALSE)
        dados <- df %>% filter(name_muni == input$municipio_censo) %>% collect()
        valores_censo$dados <- dados
        valores_censo$municipio_nome <- input$municipio_censo
        valores_censo$uf <- input$estado_censo
        return(dados)
      }, error = function(e) {
        showNotification(paste("❌ Erro ao baixar:", e$message),
                         type = "error", duration = 10)
        return(NULL)
      })
    })
  })

  output$tabela_censo <- renderDT({
    req(dados_final())
    datatable(dados_final(),
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top", rownames = FALSE)
  })

  output$dicionario_censo <- renderDT({
    req(input$dataset_censo)
    tryCatch({
      dic <- censobr::data_dictionary(year = as.numeric(input$ano_censo), dataset = "tracts")
      datatable(dic, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
    }, error = function(e) {
      showNotification("Dicionário indisponível para esta combinação.",
                       type = "warning", duration = 6)
      NULL
    })
  })

  observeEvent(input$btn_save_censo, {
    req(dados_final(), valores_censo$municipio_nome, valores_censo$uf)

    cidade_clean <- limpar_nome(valores_censo$municipio_nome)
    pasta <- file.path(pasta_dados,
                       paste0(cidade_clean, "_", valores_censo$uf),
                       paste0("ANO_", input$ano_censo), "CENSO")
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

    arquivo <- file.path(pasta,
                         paste0(cidade_clean, "_", tolower(input$dataset_censo),
                                "_", input$ano_censo, ".parquet"))

    ok <- tryCatch({
      arrow::write_parquet(dados_final(), arquivo)
      TRUE
    }, error = function(e) {
      showNotification(paste("❌ Falha ao salvar:", e$message),
                       type = "error", duration = 8)
      FALSE
    })

    if (ok) {
      showNotification(paste("✅ Salvo:", basename(arquivo)),
                       type = "message", duration = 4)
    }
  })

  output$btn_export_censo <- downloadHandler(
    filename = function() paste0("censo_", input$ano_censo, "_",
                                 input$municipio_censo, ".parquet"),
    content = function(file) {
      tryCatch(
        arrow::write_parquet(dados_final(), file),
        error = function(e) {
          writeLines(paste("Erro ao exportar:", e$message), file)
        }
      )
    }
  )
}

# 5. MÓDULO MAPAS ------------------------------------------------------------
ui_mapas <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .folder-structure {
        background-color: #2c3e50; color: #ecf0f1;
        padding: 10px; border-radius: 8px;
        font-family: monospace; font-size: 0.85em;
      }
    "))),

    fluidRow(
      column(width = 4,
        bs4Dash::bs4Card(
          title = tags$h4(icon("gear"), "Parâmetros"),
          status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,

          selectizeInput("mun_select_mapa", tags$b(icon("city"), "Município:"),
                        choices = NULL,
                        options = list(placeholder = "Digite o nome ou código...",
                                      maxOptions = 20, highlight = TRUE)),

          selectInput("camada_mapa", tags$b(icon("layer-group"), "Camada:"),
                      choices = c("Setores" = "setores", "Bairros" = "bairros",
                                 "Distritos" = "distritos", "Contorno" = "municipio")),

          selectInput("ano_mapa", tags$b(icon("calendar"), "Ano:"),
                      choices = c(2022, 2010), selected = 2022),

          hr(),

          actionButton("carregar_mapa", "Carregar da Estrutura DADOS",
                      icon = icon("folder-open"), class = "btn-warning w-100",
                      style = "margin-bottom: 10px;"),

          actionButton("download_mapa", "Buscar no IBGE",
                      icon = icon("cloud-download-alt"), class = "btn-primary w-100",
                      style = "margin-bottom: 10px;"),

          actionButton("save_mapa", "Salvar na Estrutura DADOS",
                      icon = icon("save"), class = "btn-success w-100",
                      style = "margin-bottom: 10px;"),

          sliderInput("opacidade_mapa", "Transparência:",
                      min = 0.1, max = 1, value = 0.7, step = 0.05)
        ),

        bs4Dash::bs4Card(
          title = tags$h4(icon("folder-tree"), "Estrutura"),
          status = "info", solidHeader = FALSE, width = 12,
          div(class = "folder-structure",
            "📁 DADOS/", br(),
            "  └─ 📁 CIDADE_UF/", br(),
            "      └─ 📁 ANO_2022/", br(),
            "          └─ 📁 MAPAS/", br(),
            "              └─ 🗺️ cidade_camada.parquet"
          )
        )
      ),

      column(width = 8,
        bs4Dash::bs4Card(
          title = tags$h4(icon("draw-polygon"), "Visualização"),
          status = "success", solidHeader = TRUE, width = 12, height = "700px",
          tabsetPanel(
            tabPanel("Mapa", icon = icon("map"), leafletOutput("mapa_view", height = "580px")),
            tabPanel("Tabela", icon = icon("table"), DTOutput("tabela_mapa"))
          )
        )
      )
    )
  )
}

server_mapas <- function(input, output, session) {

  valores_mapa <- reactiveValues(dados = NULL, nome = NULL, uf = NULL)

  limpar_nome <- function(nome) {
    nome_limpo <- iconv(nome, to = "ASCII//TRANSLIT")
    nome_limpo <- gsub("[^a-zA-Z0-9]", "_", nome_limpo)
    nome_limpo <- gsub("_+", "_", nome_limpo)
    nome_limpo <- tolower(nome_limpo)
    return(nome_limpo)
  }

  municipios_data <- reactiveVal(NULL)

  observe({
    tryCatch({
      muni <- read_parquet(PATH_MUNICIPIOS)
      opcoes <- muni %>%
        st_drop_geometry() %>%
        mutate(label = paste0(name_muni, " (", code_muni, ") - ", abbrev_state)) %>%
        arrange(name_muni)

      opcoes_list <- as.list(as.character(opcoes$code_muni))
      names(opcoes_list) <- opcoes$label

      municipios_data(muni)
      updateSelectizeInput(session, "mun_select_mapa",
                           choices = opcoes_list, server = TRUE)
    }, error = function(e) {
      showNotification("Não foi possível carregar lista de municípios do IBGE.",
                       type = "warning", duration = 6)
    })
  })

  obter_info <- function(cod) {
    muni <- municipios_data()
    if (is.null(muni)) return(NULL)
    info <- muni %>% st_drop_geometry() %>% filter(code_muni == as.numeric(cod))
    if (nrow(info) > 0) return(list(nome = info$name_muni[1], uf = info$abbrev_state[1]))
    return(NULL)
  }

  observeEvent(input$carregar_mapa, {
    req(input$mun_select_mapa)
    info <- obter_info(input$mun_select_mapa)
    if (is.null(info)) return()

    cidade_clean <- limpar_nome(info$nome)
    caminho <- file.path(pasta_dados,
                        paste0(cidade_clean, "_", info$uf),
                        paste0("ANO_", input$ano_mapa), "MAPAS",
                        paste0(cidade_clean, "_", input$camada_mapa,
                               "_", input$ano_mapa, ".parquet"))

    if (!file.exists(caminho)) {
      showNotification("Arquivo não encontrado! Use 'Buscar no IBGE' primeiro.",
                       type = "error", duration = 6)
      return()
    }

    dados <- sfarrow::st_read_parquet(caminho)
    if (st_crs(dados)$epsg != 4326) dados <- st_transform(dados, 4326)

    valores_mapa$dados <- dados
    valores_mapa$nome <- info$nome
    valores_mapa$uf <- info$uf

    showNotification(paste("✅", nrow(dados), "feições carregadas"),
                     type = "message", duration = 4)
  })

  observeEvent(input$download_mapa, {
    req(input$mun_select_mapa)
    info <- obter_info(input$mun_select_mapa)
    if (is.null(info)) return()

    tryCatch({
      showNotification("Baixando dados do IBGE...",
                       type = "message", duration = 5)

      res <- switch(input$camada_mapa,
        "setores" = geobr::read_census_tract(code_tract = as.numeric(input$mun_select_mapa), year = input$ano_mapa),
        "bairros" = geobr::read_neighborhood(year = input$ano_mapa) %>%
                     filter(as.numeric(code_muni) == as.numeric(input$mun_select_mapa)),
        "distritos" = geobr::read_districts(code_muni = as.numeric(input$mun_select_mapa), year = input$ano_mapa),
        "municipio" = geobr::read_municipality(code_muni = as.numeric(input$mun_select_mapa), year = input$ano_mapa)
      )

      if (st_crs(res)$epsg != 4326) res <- st_transform(res, 4326)

      valores_mapa$dados <- res
      valores_mapa$nome <- info$nome
      valores_mapa$uf <- info$uf

      showNotification(paste("✅", nrow(res), "feições baixadas"),
                       type = "message", duration = 4)

    }, error = function(e) {
      showNotification(paste("❌ Erro:", e$message),
                       type = "error", duration = 8)
    })
  })

  observeEvent(input$save_mapa, {
    req(valores_mapa$dados, valores_mapa$nome, valores_mapa$uf)

    cidade_clean <- limpar_nome(valores_mapa$nome)
    pasta <- file.path(pasta_dados,
                       paste0(cidade_clean, "_", valores_mapa$uf),
                       paste0("ANO_", input$ano_mapa), "MAPAS")
    dir.create(pasta, recursive = TRUE, showWarnings = FALSE)

    arquivo <- file.path(pasta,
                         paste0(cidade_clean, "_", input$camada_mapa,
                                "_", input$ano_mapa, ".parquet"))

    ok <- tryCatch({
      sfarrow::st_write_parquet(valores_mapa$dados, arquivo)
      TRUE
    }, error = function(e) {
      showNotification(paste("❌ Falha ao salvar:", e$message),
                       type = "error", duration = 8)
      FALSE
    })

    if (ok) {
      showNotification(paste("✅ Salvo:", basename(arquivo)),
                       type = "message", duration = 4)
    }
  })

  output$mapa_view <- renderLeaflet({
    req(valores_mapa$dados)

    cores <- c("setores" = "#198754", "bairros" = "#17a2b8",
               "distritos" = "#ffc107", "municipio" = "#6c757d")
    cor <- cores[input$camada_mapa]

    tryCatch({
      mapview(valores_mapa$dados, col.regions = cor,
              alpha.regions = input$opacidade_mapa,
              layer.name = input$camada_mapa,
              map.types = "CartoDB.Positron")@map
    }, error = function(e) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -50.65, lat = -25.46, zoom = 11)
    })
  })

  output$tabela_mapa <- renderDT({
    req(valores_mapa$dados)
    df <- valores_mapa$dados %>% st_drop_geometry()
    if (nrow(df) > 500) df <- df[1:500, ]
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
}

# 6. MÓDULO ANALISADOR -------------------------------------------------------
ui_analise <- function() {
  fluidPage(
    tags$head(tags$style(HTML("
      .city-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white; padding: 20px; border-radius: 15px; margin-bottom: 20px;
      }
      .stat-card {
        background: #f8f9fa; border-left: 4px solid #198754;
        padding: 15px; border-radius: 8px; margin-bottom: 15px;
      }
      .stat-number { font-size: 24px; font-weight: bold; color: #198754; }
    "))),

    titlePanel(h2(icon("chart-line"), "Analisador Integrado - CENSO + MAPAS",
                  style = "color: #2c3e50;")),

    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("cidade_analise", "Município:", choices = NULL, width = "100%"),
        selectInput("ano_analise", "Ano:", choices = c(2022, 2010), selected = 2022),
        hr(),
        selectInput("variavel_censo", "Variável do CENSO:", choices = NULL),
        selectInput("camada_analise", "Camada do MAPA:",
                   choices = c("setores", "bairros", "distritos", "municipio"),
                   selected = "setores"),
        hr(),
        actionButton("btn_analisar", "Executar Análise", icon = icon("play"),
                    class = "btn-success btn-block", width = "100%"),
        hr(),
        downloadButton("exportar_relatorio", "Exportar Relatório",
                      class = "btn-info btn-block", width = "100%")
      ),

      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Visão Geral", icon = icon("home"), br(), uiOutput("visao_geral")),
          tabPanel("Mapa", icon = icon("map"), br(),
                   leafletOutput("mapa_analise", height = "600px")),
          tabPanel("CENSO", icon = icon("chart-bar"), br(),
                   plotlyOutput("grafico_censo", height = "400px"), br(),
                   DTOutput("tabela_censo_analise")),
          tabPanel("Dados", icon = icon("table"), br(), DTOutput("dados_brutos"))
        )
      )
    )
  )
}

server_analise <- function(input, output, session) {

  valores <- reactiveValues(dados_censo = NULL, dados_mapas = NULL, analise_pronta = FALSE)

  limpar_nome <- function(nome) {
    nome_limpo <- iconv(nome, to = "ASCII//TRANSLIT")
    nome_limpo <- gsub("[^a-zA-Z0-9]", "_", nome_limpo)
    nome_limpo <- gsub("_+", "_", nome_limpo)
    nome_limpo <- tolower(nome_limpo)
    return(nome_limpo)
  }

  listar_cidades <- function() {
    pasta_base <- pasta_dados
    if (!dir.exists(pasta_base)) return(character(0))
    cidades <- list.dirs(pasta_base, recursive = FALSE, full.names = FALSE)
    cidades <- cidades[grepl("_.*_", cidades)]
    if (length(cidades) > 0) {
      nomes_bonitos <- sapply(cidades, function(x) {
        partes <- strsplit(x, "_")[[1]]
        cidade <- paste(partes[-length(partes)], collapse = " ")
        uf <- toupper(partes[length(partes)])
        paste0(toupper(cidade), " (", uf, ")")
      })
      names(cidades) <- nomes_bonitos
    }
    return(cidades)
  }

  observe({
    cidades <- listar_cidades()
    if (length(cidades) > 0) {
      updateSelectInput(session, "cidade_analise", choices = cidades)
    } else {
      updateSelectInput(session, "cidade_analise",
                        choices = c("(nenhuma cidade salva ainda)" = ""))
    }
  })

  observeEvent(input$cidade_analise, {
    req(input$cidade_analise, input$ano_analise)
    if (input$cidade_analise == "") return()

    partes <- strsplit(input$cidade_analise, "_")[[1]]
    uf <- toupper(partes[length(partes)])
    cidade_nome <- paste(partes[-length(partes)], collapse = " ")
    cidade_clean <- limpar_nome(cidade_nome)

    caminho_censo <- file.path(pasta_dados,
                               paste0(cidade_clean, "_", uf),
                               paste0("ANO_", input$ano_analise), "CENSO")

    if (dir.exists(caminho_censo)) {
      arquivos <- list.files(caminho_censo, pattern = "\\.parquet$", full.names = TRUE)
      if (length(arquivos) > 0) {
        dados <- arrow::read_parquet(arquivos[1])
        variaveis <- names(dados)[sapply(dados, is.numeric)]
        updateSelectInput(session, "variavel_censo",
                          choices = variaveis, selected = variaveis[1])
      }
    }
  })

  observeEvent(input$btn_analisar, {
    req(input$cidade_analise, input$ano_analise, input$camada_analise)
    if (input$cidade_analise == "") {
      showNotification("Salve primeiro dados em DADOS/ (seções CENSO e MAPAS).",
                       type = "warning", duration = 8)
      return()
    }

    partes <- strsplit(input$cidade_analise, "_")[[1]]
    uf <- toupper(partes[length(partes)])
    cidade_nome <- paste(partes[-length(partes)], collapse = " ")
    cidade_clean <- limpar_nome(cidade_nome)

    caminho_mapa <- file.path(pasta_dados,
                              paste0(cidade_clean, "_", uf),
                              paste0("ANO_", input$ano_analise), "MAPAS",
                              paste0(cidade_clean, "_", input$camada_analise,
                                     "_", input$ano_analise, ".parquet"))

    if (file.exists(caminho_mapa)) {
      valores$dados_mapas <- sfarrow::st_read_parquet(caminho_mapa)
      if (st_crs(valores$dados_mapas)$epsg != 4326) {
        valores$dados_mapas <- st_transform(valores$dados_mapas, 4326)
      }
    } else {
      showNotification("Mapa não encontrado. Salve a camada primeiro.",
                       type = "warning", duration = 6)
    }

    caminho_censo <- file.path(pasta_dados,
                               paste0(cidade_clean, "_", uf),
                               paste0("ANO_", input$ano_analise), "CENSO")
    if (dir.exists(caminho_censo)) {
      arquivos <- list.files(caminho_censo, pattern = "\\.parquet$", full.names = TRUE)
      if (length(arquivos) > 0) {
        valores$dados_censo <- arrow::read_parquet(arquivos[1])
      }
    }

    valores$analise_pronta <- TRUE
    showNotification("✅ Análise carregada!", type = "message", duration = 4)
  })

  output$visao_geral <- renderUI({
    if (!valores$analise_pronta) {
      return(div(class = "city-card",
        h3("Selecione uma cidade e clique em 'Executar Análise'")
      ))
    }

    div(
      div(class = "city-card", h2(icon("city"), " ", input$cidade_analise)),
      fluidRow(
        column(6, div(class = "stat-card",
          h4(icon("draw-polygon"), "Mapa"),
          p("Camada: ", strong(input$camada_analise)),
          p("Feições: ", strong(if (is.null(valores$dados_mapas)) "—" else nrow(valores$dados_mapas)))
        )),
        column(6, div(class = "stat-card",
          h4(icon("table"), "CENSO"),
          p("Variável: ", strong(input$variavel_censo))
        ))
      )
    )
  })

  output$mapa_analise <- renderLeaflet({
    req(valores$dados_mapas)
    tryCatch({
      mapview(valores$dados_mapas, col.regions = "#198754", alpha.regions = 0.7,
              layer.name = input$camada_analise,
              map.types = "CartoDB.Positron")@map
    }, error = function(e) {
      leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -50.65, lat = -25.46, zoom = 11)
    })
  })

  output$grafico_censo <- renderPlotly({
    req(valores$dados_censo, input$variavel_censo)
    if (input$variavel_censo %in% names(valores$dados_censo)) {
      plot_ly() %>%
        add_histogram(x = valores$dados_censo[[input$variavel_censo]],
                     marker = list(color = "#198754")) %>%
        layout(title = paste("Distribuição de", input$variavel_censo),
               xaxis = list(title = input$variavel_censo))
    }
  })

  output$tabela_censo_analise <- renderDT({
    req(valores$dados_censo)
    datatable(valores$dados_censo,
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$dados_brutos <- renderDT({
    req(valores$dados_mapas)
    df <- valores$dados_mapas %>% st_drop_geometry()
    if (nrow(df) > 500) df <- df[1:500, ]
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$exportar_relatorio <- downloadHandler(
    filename = function() paste0("relatorio_",
                                 gsub(" ", "_", input$cidade_analise), ".html"),
    content = function(file) {
      html <- sprintf(
        "<html><body><h1>Relatório - %s</h1><p>Data: %s</p>
         <p>Ano: %s</p><p>Camada: %s</p><p>Feições: %d</p></body></html>",
        input$cidade_analise, Sys.time(), input$ano_analise,
        input$camada_analise,
        if (is.null(valores$dados_mapas)) 0 else nrow(valores$dados_mapas))
      writeLines(html, file)
    }
  )
}

# 7. MENU PRINCIPAL ----------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = tags$div(icon("chart-line"), " Irati Analytics 2026")),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Censo IBGE",        tabName = "tab_censo",   icon = icon("users-gear")),
      menuItem("Mapas",             tabName = "tab_mapas",   icon = icon("map-location-dot")),
      menuItem("Análise Espacial",  tabName = "tab_analise", icon = icon("chart-line")),
      menuItem("Manual",            tabName = "tab_manual",  icon = icon("book-open"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(".content-wrapper, .right-side { background-color: #f4f6f9; }")),
    tabItems(
      tabItem(tabName = "tab_censo",   ui_censo()),
      tabItem(tabName = "tab_mapas",   ui_mapas()),
      tabItem(tabName = "tab_analise", ui_analise()),
      tabItem(tabName = "tab_manual",  ui_manual())
    )
  )
)

server <- function(input, output, session) {

  server_censo(input, output, session)
  server_mapas(input, output, session)
  server_analise(input, output, session)

  # 🎉 MODAL DE BOAS-VINDAS NA PRIMEIRA EXECUÇÃO -------------------------
  # Marca se já mostrou o modal nesta sessão
  ja_mostrou <- reactiveVal(FALSE)

  observe({
    if (!ja_mostrou()) {
      ja_mostrou(TRUE)

      # Só mostra o modal se o usuário NÃO estiver na aba Manual
      showModal(modalDialog(
        title = tags$div(icon("hand-wave"), " Bem-vindo ao Irati Analytics 2026!"),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton("ir_manual", "📖 Abrir Manual", class = "btn-info"),
          modalButton("✅ Entendi, começar")
        ),
        tags$div(
          class = "manual-box",
          style = "border-left-color:#198754;",
          tags$p(tags$b("Este é um app standalone."),
                 "Tudo roda localmente no seu computador."),
          tags$ul(
            tags$li("Use o menu lateral para navegar entre ", strong("Censo"), ", ",
                    strong("Mapas"), ", ", strong("Análise Espacial"), " e ", strong("Manual"), "."),
            tags$li("Na ", strong("primeira execução"), ", os pacotes serão instalados automaticamente —",
                    " isso pode levar alguns minutos."),
            tags$li("Os dados são salvos na pasta ", code("DADOS/"),
                    " ao lado do arquivo ", code("app.R"), "."),
            tags$li("Se algo travar, consulte a aba ", strong("Manual"), " para solução de problemas.")
          ),
          tags$hr(),
          tags$p(style = "color:#6c757d;font-size:0.9em;",
                 icon("circle-info"), " Você pode reabrir este manual a qualquer momento.",
                 " Não mostramos este aviso novamente nesta sessão.")
        )
      ))
    }
  })

  # Botão "Abrir Manual" do modal leva direto à aba Manual
  observeEvent(input$ir_manual, {
    removeModal()
    updateTabItems(session, "tabs", "tab_manual")
  })

  # Notificação de carregamento concluído
  showNotification("✅ Sistema Irati Analytics 2026 carregado!",
                   type = "message", duration = 4)
}

# 8. INICIALIZAÇÃO -----------------------------------------------------------
# Abre automaticamente no navegador padrão
shinyApp(ui, server, options = list(launch.browser = TRUE))