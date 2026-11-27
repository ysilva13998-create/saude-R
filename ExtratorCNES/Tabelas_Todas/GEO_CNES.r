# ==============================================================================
# GEO_CNES.R - Editor de Coordenadas CNES com DuckDB e Filtro por Município
# VERSÃO STANDALONE - Executa por si mesmo
# ==============================================================================

library(shiny)
library(DT)
library(dplyr)
library(jsonlite)
library(arrow)
library(leaflet)
library(leaflet.extras)
library(geobr)
library(sf)
library(DBI)
library(duckdb)
library(stringr)

# ==============================================================================
# 1. DESCOBERTA DA PASTA DO APP / RAIZ EXTRAÍDA
# ==============================================================================

obter_pasta_app_geo <- function() {
  # 1. Execução via Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }

  # 2. Execução via source() (ex.: .bat ou editor)
  if (sys.nframe() > 0) {
    for (i in rev(seq_len(sys.nframe()))) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nzchar(ofile)) {
        return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
      }
    }
  }

  # 3. Fallback: diretório atual
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

pasta_app <- obter_pasta_app_geo()
pasta_raiz <- if (tolower(basename(pasta_app)) %in% c("extratorcnes", "extratorsinasc")) {
  dirname(pasta_app)
} else {
  pasta_app
}

# Localiza a pasta 'dicionarios' na raiz extraída (ou dentro de pasta_app)
candidatos_dict_dir <- unique(c(
  file.path(pasta_raiz, "dicionarios"),
  file.path(pasta_app, "dicionarios")
))
pasta_dicionarios <- candidatos_dict_dir[dir.exists(candidatos_dict_dir)][1]
if (is.na(pasta_dicionarios) || !dir.exists(pasta_dicionarios)) {
  pasta_dicionarios <- file.path(pasta_raiz, "dicionarios")
  dir.create(pasta_dicionarios, recursive = TRUE, showWarnings = FALSE)
}
pasta_dicionarios <- normalizePath(pasta_dicionarios, winslash = "/", mustWork = FALSE)

# Caminho do cnes.duckdb dentro de dicionarios
caminho_cnes_duckdb <- file.path(pasta_dicionarios, "cnes.duckdb")
if (!file.exists(caminho_cnes_duckdb)) {
  stop("Não foi possível encontrar 'cnes.duckdb' em: ", caminho_cnes_duckdb)
}
caminho_cnes_duckdb <- normalizePath(caminho_cnes_duckdb, winslash = "/", mustWork = TRUE)

# ==============================================================================
# 2. CONFIGURAÇÃO E CONSTANTES
# ==============================================================================

# Conexão com DuckDB em modo edição (read_only = FALSE) apontando para dicionarios/cnes.duckdb
con_duck <- dbConnect(
  duckdb::duckdb(),
  dbdir = caminho_cnes_duckdb,
  read_only = FALSE
)

# Pasta base de tabelas (procura em ExtratorCNES, ExtratorSINASC ou na raiz extraída)
candidatos_tabelas <- unique(c(
  file.path(pasta_app, "Tabelas_Todas"),
  file.path(pasta_raiz, "ExtratorSINASC", "Tabelas_Todas"),
  file.path(pasta_raiz, "Tabelas_Todas")
))
pasta_tabelas <- candidatos_tabelas[dir.exists(candidatos_tabelas)][1]
if (is.na(pasta_tabelas) || !dir.exists(pasta_tabelas)) {
  pasta_tabelas <- file.path(pasta_app, "Tabelas_Todas")
}
pasta_tabelas <- paste0(normalizePath(pasta_tabelas, winslash = "/", mustWork = FALSE), "/")

PATH_CONFIG_GEO <- list(
  raiz = pasta_tabelas
)

# Configuração CNES e auxiliares
PASTA_CNES <- paste0(PATH_CONFIG_GEO$raiz, "CNES/FINAL")
PASTA_MAPAS <- paste0(PATH_CONFIG_GEO$raiz, "APP_GERADOR/mapas_offline/")
PATH_MUNICIPIOS <- paste0(PATH_CONFIG_GEO$raiz, "_METADADOS/br_bd_diretorios_brasil_municipio.parquet")

# Criar pastas se não existirem
if (!dir.exists(PASTA_CNES)) dir.create(PASTA_CNES, recursive = TRUE)
if (!dir.exists(PASTA_MAPAS)) dir.create(PASTA_MAPAS, recursive = TRUE)
# ==============================================================================
# 2. FUNÇÕES AUXILIARES
# ==============================================================================

# Função para formatar números
format_number <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Função para carregar dados completos do DuckDB com opção de filtro por município
carregar_dados_completos <- function(codigo_municipio = NULL) {
  
  # Base da query
  query <- "
    SELECT
      CNES,
      NOME_ESTABELECIMENTO,
      lat,
      long,
      CODIGO_MUNICIPIO
    FROM cnes
  "
  
  # Adicionar filtro por município se fornecido
  if(!is.null(codigo_municipio) && codigo_municipio != "") {
    query <- paste0(
      query,
      " WHERE CODIGO_MUNICIPIO = ",
      DBI::dbQuoteString(con_duck, as.character(codigo_municipio))
    )
  }
  
  query <- paste0(query, " ORDER BY CNES")
  
  # Carregar do DuckDB
  df <- dbGetQuery(con_duck, query)
  
  if(nrow(df) == 0) {
    return(data.frame(
      CNES = character(),
      NOME_ESTABELECIMENTO = character(),
      lat = numeric(),
      long = numeric(),
      CODIGO_MUNICIPIO = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Converter para numérico (garantir que é double)
  df$lat <- as.numeric(df$lat)
  df$long <- as.numeric(df$long)
  
  return(df)
}

# Função para carregar municípios do DuckDB (distinct)
carregar_municipios <- function() {
  municipios <- dbGetQuery(con_duck, "
    SELECT DISTINCT
      CODIGO_MUNICIPIO AS codigo,
      CODIGO_MUNICIPIO AS nome
    FROM cnes
    WHERE CODIGO_MUNICIPIO IS NOT NULL
      AND CODIGO_MUNICIPIO != ''
    ORDER BY CODIGO_MUNICIPIO
  ")
  return(municipios)
}

# Função para salvar dados no DuckDB
salvar_dados_duckdb <- function(df) {
  
  # Preparar dados para atualizar no DuckDB
  df_save <- df %>% 
    dplyr::select(CNES, NOME_ESTABELECIMENTO, lat, long, CODIGO_MUNICIPIO) %>%
    dplyr::filter(!is.na(CNES) & CNES != "")
  
  # Atualizar cada registro individualmente
  for(i in 1:nrow(df_save)) {
    dbExecute(con_duck, "
      UPDATE cnes
      SET lat = ?,
          long = ?,
          NOME_ESTABELECIMENTO = ?,
          CODIGO_MUNICIPIO = ?
      WHERE CNES = ?
    ", list(
      ifelse(is.na(df_save$lat[i]), NA, df_save$lat[i]),
      ifelse(is.na(df_save$long[i]), NA, df_save$long[i]),
      df_save$NOME_ESTABELECIMENTO[i],
      df_save$CODIGO_MUNICIPIO[i],
      df_save$CNES[i]
    ))
  }
  
  # Verificar se algum CNES não existe na tabela (para inserir)
  cnes_existentes <- dbGetQuery(con_duck, "SELECT cnes FROM cnes")$cnes
  novos_cnes <- df_save %>% filter(!CNES %in% cnes_existentes)
  
  if(nrow(novos_cnes) > 0) {
    for(i in 1:nrow(novos_cnes)) {
      dbExecute(con_duck, "
        INSERT INTO cnes
          (CNES, NOME_ESTABELECIMENTO, lat, long, CODIGO_MUNICIPIO)
        VALUES (?, ?, ?, ?, ?)
      ", list(
        novos_cnes$CNES[i],
        novos_cnes$NOME_ESTABELECIMENTO[i],
        ifelse(is.na(novos_cnes$lat[i]), NA, novos_cnes$lat[i]),
        ifelse(is.na(novos_cnes$long[i]), NA, novos_cnes$long[i]),
        novos_cnes$CODIGO_MUNICIPIO[i]
      ))
    }
  }
  
  return(TRUE)
}

# Função para obter nome do município a partir do código
get_nome_municipio <- function(codigo) {
  tryCatch({
    if(file.exists(PATH_MUNICIPIOS)) {
      municipios <- read_parquet(PATH_MUNICIPIOS)
      codigo_formatado <- str_pad(as.character(codigo), 6, pad = "0")
      nome <- municipios %>%
        filter(id_municipio_6 == codigo_formatado) %>%
        pull(nome) %>%
        first()
      return(nome)
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

# Função para baixar mapa do IBGE
baixar_mapa_ibge <- function(uf = "PR", ano = 2020) {
  withProgress(message = 'Baixando contorno do IBGE...', value = 0, {
    incProgress(0.3, detail = "Conectando ao IBGE...")
    
    tryCatch({
      suppressWarnings({
        mapa <- geobr::read_municipality(code_muni = uf, year = ano)
      })
      
      if(sf::st_crs(mapa)$epsg != 4326) {
        mapa <- sf::st_transform(mapa, 4326)
      }
      
      incProgress(0.6, detail = "Processando dados...")
      
      arquivo_rds <- paste0(PASTA_MAPAS, "contorno_", uf, "_", ano, ".rds")
      saveRDS(mapa, arquivo_rds)
      
      incProgress(1.0, detail = "Concluído!")
      
      return(list(success = TRUE, arquivo = arquivo_rds, mapa = mapa))
      
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  })
}

# Função para carregar contorno offline
carregar_contorno_offline <- function(uf = "PR", ano = 2020) {
  arquivo <- paste0(PASTA_MAPAS, "contorno_", uf, "_", ano, ".rds")
  if(file.exists(arquivo)) {
    mapa <- readRDS(arquivo)
    if(sf::st_crs(mapa)$epsg != 4326) {
      mapa <- sf::st_transform(mapa, 4326)
    }
    return(mapa)
  }
  return(NULL)
}

# ==============================================================================
# 3. INTERFACE (UI)
# ==============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .top-box-geo { 
        background: linear-gradient(135deg, #28a745, #1e7e34); 
        color: white; 
        padding: 20px; 
        border-radius: 8px; 
        text-align: center; 
        margin-bottom: 20px;
      }
      
      .stat-card-geo { 
        background: white; 
        border-radius: 8px; 
        padding: 15px; 
        text-align: center; 
        border: 1px solid #e0e0e0;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        transition: transform 0.3s ease;
      }
      
      .stat-card-geo:hover {
        transform: translateY(-2px);
      }
      
      .stat-number-geo { 
        font-size: 28px; 
        font-weight: bold; 
        color: #28a745; 
        margin-top: 10px;
      }
      
      .info-box-geo {
        background: #e8f5e9;
        border-left: 4px solid #28a745;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
      }
      
      .offline-badge {
        background: #ffc107;
        color: #856404;
        padding: 5px 10px;
        border-radius: 20px;
        font-size: 12px;
        display: inline-block;
      }
      
      .online-badge {
        background: #28a745;
        color: white;
        padding: 5px 10px;
        border-radius: 20px;
        font-size: 12px;
        display: inline-block;
      }
      
      .download-btn, .load-btn {
        background: #17a2b8;
        color: white;
        border: none;
        padding: 5px 10px;
        border-radius: 5px;
        font-size: 12px;
        cursor: pointer;
      }
      
      .download-btn:hover, .load-btn:hover {
        background: #138496;
      }
      
      .btn-group-inline {
        display: flex;
        gap: 5px;
        align-items: center;
      }
      
      .duckdb-badge {
        background: #4caf50;
        color: white;
        padding: 3px 8px;
        border-radius: 4px;
        font-size: 11px;
        margin-left: 10px;
      }
      
      .filter-box {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 15px;
      }
      
      .auto-reload-badge {
        background: #17a2b8;
        color: white;
        padding: 2px 8px;
        border-radius: 20px;
        font-size: 10px;
        margin-left: 10px;
      }
    "))
  ),

  div(class = "top-box-geo",
      h2(icon("map-marked-alt"), " Editor de Coordenadas CNES"),
      p("Gerenciamento de georreferenciamento para mapas de saúde"),
      span(class = "duckdb-badge", icon("database"), " DuckDB"),
      span(class = "auto-reload-badge", icon("sync-alt"), " Auto-recarga a cada 30s")
  ),

  fluidRow(
    column(3, 
      div(class = "stat-card-geo", 
        div(icon("building"), " Total"),
        div(class = "stat-number-geo", textOutput("total_geo"))
      )
    ),
    column(3, 
      div(class = "stat-card-geo", 
        div(icon("check-circle"), " Com Coordenadas"),
        div(class = "stat-number-geo", textOutput("ok_geo"))
      )
    ),
    column(3, 
      div(class = "stat-card-geo", 
        div(icon("exclamation-triangle"), " Pendentes"),
        div(class = "stat-number-geo", textOutput("faltando_geo"))
      )
    ),
    column(3,
      div(class = "stat-card-geo",
        div(icon("map"), " No Mapa"),
        div(class = "stat-number-geo", textOutput("no_mapa_geo"))
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(12,
      div(class = "info-box-geo",
        icon("info-circle"),
        " Modo: ",
        span(class = "offline-badge", icon("wifi"), " OFFLINE - Ruas funcionam sem internet!"),
        " | ",
        span(id = "status_contorno", class = "offline-badge", icon("draw-polygon"), " Contorno: Verificando..."),
        " | ",
        span(icon("database"), " Dados salvos no DuckDB")
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(4,
      wellPanel(
        h5(icon("sliders-h"), " Controles"),
        
        # Filtro por município (código CODUFMUN)
        div(class = "filter-box",
          h6(icon("city"), " Filtro por Município (CODUFMUN)"),
          selectizeInput(
            "filtro_municipio_geo",
            label = "Código do Município:",
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Selecione um código de município...",
              allowEmptyOption = TRUE
            )
          ),
          actionButton("limpar_filtro", "Limpar Filtro", 
                      icon = icon("eraser"),
                      style = "width: 100%; margin-top: 5px;")
        ),
        
        selectInput("filtro_geo", "Status:", 
                   choices = c("Todos" = "todos", 
                              "Com Coordenadas" = "preenchidos", 
                              "Sem Coordenadas" = "pendentes")),
        
        selectInput("busca_tipo", "Buscar por:", 
                   choices = c("CNES" = "cnes", "Nome" = "nome")),
        
        textInput("busca_texto", "Buscar:", placeholder = "Digite CNES ou nome..."),
        
        actionButton("recarregar_geo", "Recarregar do DuckDB", 
                    class = "btn-warning", 
                    icon = icon("sync"),
                    style = "width: 100%; margin-bottom: 10px; background-color: #ffc107; color: #856404;"),
        
        actionButton("salvar_geo", "Salvar no DuckDB", 
                    class = "btn-success", 
                    icon = icon("save"),
                    style = "width: 100%;")
      ),
      
      wellPanel(
        h5(icon("map"), " Contorno IBGE"),
        div(class = "btn-group-inline",
          selectInput("uf_contorno", "UF:", 
                     choices = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO"),
                     width = "70px"),
          numericInput("ano_contorno", "Ano:", value = 2020, min = 2010, max = 2024, width = "80px"),
          actionButton("baixar_contorno", icon("download"), 
                      class = "download-btn",
                      title = "Baixar contorno do IBGE"),
          actionButton("carregar_contorno", icon("upload"), 
                      class = "load-btn",
                      title = "Carregar contorno baixado")
        ),
        p(style = "font-size: 11px; margin-top: 5px; color: #666;",
          icon("info-circle"), " Baixe uma vez, use offline")
      )
    ),
    
    column(8,
      wellPanel(
        h5(icon("map"), " Mapa das Unidades"),
        div(class = "info-box-geo", style = "margin-bottom: 10px;",
          icon("mouse-pointer"),
          " Clique no mapa para obter coordenadas | ",
          icon("download"), " Baixe contorno do IBGE para ver limites municipais"
        ),
        leafletOutput("mapa_geo", height = "500px")
      )
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12,
      wellPanel(
        h5(icon("edit"), " Editor de Coordenadas"),
        p(style = "color: #666; font-size: 12px;", 
          icon("info-circle"), " Dica: Clique duas vezes em uma célula para editar. Use números decimais para coordenadas."),
        p(style = "color: #4caf50; font-size: 12px;", 
          icon("check-circle"), " O nome do estabelecimento é editável e será salvo no DuckDB!"),
        DTOutput("tabela_geo")
      )
    )
  )
)

# ==============================================================================
# 4. LÓGICA DO SERVIDOR (SERVER)
# ==============================================================================

server <- function(input, output, session) {
  
  dados_geo <- reactiveVal()
  dados_completos <- reactiveVal()
  temp_coords <- reactiveVal(list(lat = NULL, lng = NULL))
  contorno_atual <- reactiveVal(NULL)
  
  # Timer para recarga automática a cada 30 segundos
  auto_recarga_timer <- reactiveTimer(30000)
  
  # Carregar lista de municípios disponíveis
  observe({
    municipios <- carregar_municipios()
    
    if(nrow(municipios) > 0) {
      opcoes <- setNames(municipios$codigo, municipios$codigo)
      
      updateSelectizeInput(
        session, "filtro_municipio_geo",
        choices = c("(Todos)" = "", opcoes),
        server = TRUE
      )
    }
  })

  # Função para carregar base completa
  carregar_base_geo <- function(codigo_municipio = NULL) {
    carregar_dados_completos(codigo_municipio)
  }
  
  # Função para recarregar dados automaticamente
  recarregar_automatico <- function() {
    df <- carregar_base_geo()
    dados_completos(df)
    
    if(!is.null(input$filtro_municipio_geo) && input$filtro_municipio_geo != "") {
      df <- df %>% dplyr::filter(CODIGO_MUNICIPIO == input$filtro_municipio_geo)
    }
    dados_geo(df)
    
    total <- nrow(df)
    com_coords <- sum(!is.na(df$lat) & !is.na(df$long))
    message(paste("🔄 Auto-recarga:", total, "CNES |", com_coords, "com coordenadas"))
  }
  
  # Observador para recarga automática
  observeEvent(auto_recarga_timer(), {
    recarregar_automatico()
  })
  
  # Carregar todos os dados inicialmente
  observe({ 
    df <- carregar_base_geo()
    dados_completos(df)
    dados_geo(df)
    
    # Verificar disponibilidade do contorno
    uf_padrao <- "PR"
    if(file.exists(paste0(PASTA_MAPAS, "contorno_", uf_padrao, "_2020.rds"))) {
      updateContornoStatus("disponível")
    } else {
      updateContornoStatus("não disponível")
    }
  })
  
  # Filtrar por município quando selecionado
  observeEvent(input$filtro_municipio_geo, {
    req(dados_completos())
    
    if(input$filtro_municipio_geo != "") {
      df_filtrado <- dados_completos() %>%
        dplyr::filter(CODIGO_MUNICIPIO == input$filtro_municipio_geo)
      
      dados_geo(df_filtrado)
      
      nome_muni <- get_nome_municipio(input$filtro_municipio_geo)
      if(!is.null(nome_muni)) {
        showNotification(paste("Filtrando município:", nome_muni, "(", input$filtro_municipio_geo, ")"), 
                        type = "message", duration = 2)
      } else {
        showNotification(paste("Filtrando código:", input$filtro_municipio_geo), 
                        type = "message", duration = 2)
      }
    } else {
      dados_geo(dados_completos())
      showNotification("Filtro removido - mostrando todos os CNES", type = "message", duration = 2)
    }
  })
  
  # Limpar filtro
  observeEvent(input$limpar_filtro, {
    updateSelectizeInput(session, "filtro_municipio_geo", selected = "")
    dados_geo(dados_completos())
  })
  
  # Função para atualizar o status do contorno na UI
  updateContornoStatus <- function(status) {
    session$sendCustomMessage("updateContornoStatus", status)
  }
  
  # JavaScript para atualizar o status visualmente
  session$onFlushed(function() {
    session$sendCustomMessage("contornoStatusHandler", 
      'Shiny.addCustomMessageHandler("updateContornoStatus", function(status) {
        var badge = document.getElementById("status_contorno");
        if(badge) {
          if(status === "disponível") {
            badge.innerHTML = "<i class=\\"fa fa-check-circle\\"></i> Contorno: Disponível";
            badge.style.background = "#28a745";
            badge.style.color = "white";
          } else {
            badge.innerHTML = "<i class=\\"fa fa-download\\"></i> Contorno: Baixar para usar";
            badge.style.background = "#ffc107";
            badge.style.color = "#856404";
          }
        }
      });'
    )
  })

  # RECARREGAR do DuckDB (com confirmação)
  observeEvent(input$recarregar_geo, {
    showModal(modalDialog(
      title = "⚠️ Confirmar recarga",
      div(
        icon("exclamation-triangle", class = "fa-2x", style = "color: #ffc107;"),
        tags$br(),
        "Recarregar do banco de dados irá",
        tags$strong(tags$span(style = "color: #d9534f;", " PERDER TODAS as alterações NÃO salvas")),
        ".",
        tags$br(),
        tags$br(),
        "Tem certeza que deseja continuar?"
      ),
      footer = tagList(
        actionButton("confirmar_recarga", "Sim, recarregar", class = "btn-danger"),
        modalButton("Cancelar")
      ),
      easyClose = TRUE
    ))
  })
  
  # Confirmar recarga
  observeEvent(input$confirmar_recarga, {
    removeModal()
    
    df <- carregar_base_geo()
    dados_completos(df)
    
    if(!is.null(input$filtro_municipio_geo) && input$filtro_municipio_geo != "") {
      df <- df %>% dplyr::filter(CODIGO_MUNICIPIO == input$filtro_municipio_geo)
    }
    dados_geo(df)
    
    showNotification("Base recarregada do DuckDB. Alterações não salvas foram perdidas.", 
                    type = "warning", duration = 3)
  })
  
  # =====================================================
  # BAIXAR CONTORNO DO IBGE
  # =====================================================
  observeEvent(input$baixar_contorno, {
    resultado <- baixar_mapa_ibge(input$uf_contorno, input$ano_contorno)
    
    if(resultado$success) {
      showNotification(paste("Contorno do", input$uf_contorno, "baixado com sucesso!"), 
                      type = "message", duration = 3)
      updateContornoStatus("disponível")
    } else {
      showNotification(paste("Erro ao baixar:", resultado$error), type = "error")
    }
  })
  
  # =====================================================
  # CARREGAR CONTORNO OFFLINE
  # =====================================================
  observeEvent(input$carregar_contorno, {
    mapa <- carregar_contorno_offline(input$uf_contorno, input$ano_contorno)
    
    if(!is.null(mapa)) {
      contorno_atual(mapa)
      updateContornoStatus("disponível")
      
      showNotification(paste("Contorno do", input$uf_contorno, "carregado!"), 
                      type = "message", duration = 2)
      
      leafletProxy("mapa_geo") %>%
        clearGroup("contorno") %>%
        addPolygons(
          data = mapa,
          fillColor = "transparent",
          fillOpacity = 0,
          color = "#ff6600",
          weight = 2,
          opacity = 0.8,
          label = ~name_muni,
          popup = ~paste0("<b>", name_muni, "</b>"),
          group = "contorno"
        )
    } else {
      showNotification(paste("Contorno do", input$uf_contorno, "não encontrado! Baixe primeiro."), 
                      type = "warning", duration = 3)
    }
  })

  dados_filtrados_geo <- reactive({
    req(dados_geo())
    df <- dados_geo()
    
    if (input$filtro_geo == "pendentes") {
      df <- df %>% dplyr::filter(is.na(lat) | is.na(long))
    } else if (input$filtro_geo == "preenchidos") {
      df <- df %>% dplyr::filter(!is.na(lat) & !is.na(long))
    }
    
    if (!is.null(input$busca_texto) && input$busca_texto != "") {
      busca <- toupper(trimws(input$busca_texto))
      if (input$busca_tipo == "cnes") {
        df <- df %>% dplyr::filter(grepl(busca, toupper(CNES)))
      } else if (input$busca_tipo == "nome") {
        df <- df %>% dplyr::filter(grepl(busca, toupper(NOME_ESTABELECIMENTO)))
      }
    }
    
    df
  })

  # =====================================================
  # MAPA COM RUAS
  # =====================================================
  output$mapa_geo <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles(
        urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
        attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a> | &copy; <a href="https://carto.com/">CARTO</a>'
      ) %>%
      addScaleBar(position = "bottomleft") %>%
      addResetMapButton() %>%
      setView(lng = -51.9253, lat = -24.9459, zoom = 6)
  })
  
  # Atualiza os marcadores no mapa
  observe({
    df <- dados_filtrados_geo()
    df_com_coords <- df %>% dplyr::filter(!is.na(lat) & !is.na(long))
    
    leafletProxy("mapa_geo") %>%
      clearMarkers() %>%
      clearGroup("estabelecimentos")
    
    if(nrow(df_com_coords) > 0) {
      leafletProxy("mapa_geo") %>%
        addCircleMarkers(
          data = df_com_coords,
          lng = ~long,
          lat = ~lat,
          radius = 6,
          color = "#28a745",
          fillColor = "#28a745",
          fillOpacity = 0.8,
          popup = ~paste0(
            "<b>", ifelse(is.na(NOME_ESTABELECIMENTO), "Sem nome", NOME_ESTABELECIMENTO), "</b><br>",
            "<b>CNES:</b> ", CNES, "<br>",
            "<b>Código Município:</b> ", CODIGO_MUNICIPIO, "<br>",
            "<b>Lat:</b> ", round(lat, 6), "<br>",
            "<b>Long:</b> ", round(long, 6)
          ),
          label = ~ifelse(is.na(NOME_ESTABELECIMENTO), CNES, NOME_ESTABELECIMENTO),
          group = "estabelecimentos"
        )
    }
  })
  
  # Capturar clique no mapa
  observeEvent(input$mapa_geo_click, {
    click <- input$mapa_geo_click
    temp_coords(list(lat = click$lat, lng = click$lng))
    
    leafletProxy("mapa_geo") %>%
      clearGroup("clique") %>%
      addCircleMarkers(
        lng = click$lng,
        lat = click$lat,
        radius = 8,
        color = "#ff6600",
        fillColor = "#ff6600",
        fillOpacity = 0.8,
        popup = paste0("Coordenadas selecionadas:<br>Lat: ", round(click$lat, 6), "<br>Long: ", round(click$lng, 6)),
        group = "clique"
      )
    
    showModal(modalDialog(
      title = "Coordenadas selecionadas",
      paste("Latitude:", round(click$lat, 6), 
            "\nLongitude:", round(click$lng, 6)),
      footer = tagList(
        actionButton("usar_coord", "Usar estas coordenadas", class = "btn-success"),
        modalButton("Cancelar")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$usar_coord, {
    removeModal()
    showModal(modalDialog(
      title = "Aplicar coordenadas",
      selectizeInput("cnes_selecionado", "Selecione o CNES:", 
                    choices = dados_filtrados_geo()$CNES,
                    options = list(placeholder = "Digite para buscar...")),
      footer = tagList(
        actionButton("aplicar_coord", "Aplicar", class = "btn-primary"),
        modalButton("Cancelar")
      )
    ))
  })
  
  observeEvent(input$aplicar_coord, {
    req(input$cnes_selecionado)
    coords <- temp_coords()
    
    if(!is.null(coords$lat) && !is.null(coords$lng)) {
      df <- dados_geo()
      linha <- which(df$CNES == input$cnes_selecionado)
      
      if(length(linha) > 0) {
        df$lat[linha] <- coords$lat
        df$long[linha] <- coords$lng
        dados_geo(df)
        
        df_completo <- dados_completos()
        linha_completa <- which(df_completo$CNES == input$cnes_selecionado)
        if(length(linha_completa) > 0) {
          df_completo$lat[linha_completa] <- coords$lat
          df_completo$long[linha_completa] <- coords$lng
          dados_completos(df_completo)
        }
        
        showNotification(paste("Coordenadas aplicadas ao CNES", input$cnes_selecionado), 
                        type = "message")
        
        leafletProxy("mapa_geo") %>% clearGroup("clique")
      } else {
        showNotification("CNES não encontrado!", type = "warning")
      }
    }
    removeModal()
  })

  # Tabela editável
  output$tabela_geo <- renderDT({
    df_display <- dados_filtrados_geo()
    colunas_ordem <- c("CNES", "NOME_ESTABELECIMENTO", "CODIGO_MUNICIPIO", "lat", "long")
    colunas_existentes <- colunas_ordem[colunas_ordem %in% names(df_display)]
    df_display <- df_display %>% dplyr::select(dplyr::all_of(colunas_existentes))
    
    datatable(df_display, 
              editable = list(target = "cell", disable = list(columns = c(0, 2))),
              options = list(
                pageLength = 10, 
                scrollX = TRUE,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
              ), 
              rownames = FALSE)
  })

  # Editar célula
  observeEvent(input$tabela_geo_cell_edit, {
    info <- input$tabela_geo_cell_edit
    df <- dados_geo()
    
    df_filtrado <- dados_filtrados_geo()
    cnes_editado <- df_filtrado$CNES[info$row]
    linha_real <- which(df$CNES == cnes_editado)
    
    colunas_ordem <- c("CNES", "NOME_ESTABELECIMENTO", "CODIGO_MUNICIPIO", "lat", "long")
    colunas_existentes <- colunas_ordem[colunas_ordem %in% names(df_filtrado)]
    coluna <- colunas_existentes[info$col + 1]
    
    if(coluna %in% c("lat", "long")) {
      novo_valor <- as.numeric(info$value)
      if(!is.na(novo_valor)) {
        df[linha_real, coluna] <- novo_valor
        dados_geo(df)
        
        df_completo <- dados_completos()
        linha_completa <- which(df_completo$CNES == cnes_editado)
        if(length(linha_completa) > 0) {
          df_completo[linha_completa, coluna] <- novo_valor
          dados_completos(df_completo)
        }
        
        showNotification(paste(coluna, "atualizado para", novo_valor), type = "message")
      } else {
        showNotification("Valor inválido! Use números decimais (ex: -23.5505)", 
                        type = "warning")
      }
    } else if(coluna == "NOME_ESTABELECIMENTO") {
      novo_valor <- as.character(info$value)
      df[linha_real, coluna] <- novo_valor
      dados_geo(df)
      
      df_completo <- dados_completos()
      linha_completa <- which(df_completo$CNES == cnes_editado)
      if(length(linha_completa) > 0) {
        df_completo[linha_completa, coluna] <- novo_valor
        dados_completos(df_completo)
      }
      
      showNotification(paste("Nome atualizado para:", novo_valor), type = "message")
    }
  })

  # Salvar no DuckDB
  observeEvent(input$salvar_geo, {
    req(dados_completos())
    df_save <- dados_completos()
    
    resultado <- salvar_dados_duckdb(df_save)
    
    if(resultado) {
      showNotification(paste("✅ Dados salvos no DuckDB!", 
                            sum(!is.na(df_save$lat) & !is.na(df_save$long)), 
                            "estabelecimentos com coordenadas"), 
                      type = "message", duration = 3)
    } else {
      showNotification("❌ Erro ao salvar no DuckDB", type = "error")
    }
  })

  # Estatísticas
  output$total_geo <- renderText({ 
    req(dados_geo())
    format_number(nrow(dados_geo()))
  })
  
  output$ok_geo <- renderText({ 
    req(dados_geo())
    format_number(sum(!is.na(dados_geo()$lat) & !is.na(dados_geo()$long)))
  })
  
  output$faltando_geo <- renderText({ 
    req(dados_geo())
    format_number(sum(is.na(dados_geo()$lat) | is.na(dados_geo()$long)))
  })
  
  output$no_mapa_geo <- renderText({ 
    req(dados_filtrados_geo())
    df <- dados_filtrados_geo()
    format_number(sum(!is.na(df$lat) & !is.na(df$long)))
  })
}

# ==============================================================================
# 5. EXECUÇÃO
# ==============================================================================

options(shiny.launch.browser = TRUE)

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)

# Fechar conexão quando o app fechar
onStop(function() {
  dbDisconnect(con_duck)
})
