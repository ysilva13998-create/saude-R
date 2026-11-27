library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(purrr)

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

  # 3. Fallback: getwd() (mesmo comportamento do extrator)
  return(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
}

# ==============================================================================
# RAIZ DO APP E DA PASTA EXTRAÍDA
# ==============================================================================
pasta_app <- obter_pasta_app()
pasta_raiz <- if (tolower(basename(pasta_app)) == "extratorsinasc") {
  dirname(pasta_app)
} else {
  pasta_app
}

# ==============================================================================
# CONEXÃO DUCKDB (PRINCIPAL)
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

municipios_df <- dbGetQuery(con_dict, "
    SELECT 
        id_municipio_6 as id6,
        nome,
        sigla_uf
    FROM municipios
")
municipios_df <- as.data.frame(municipios_df)

# ==============================================================================
# LOCALIZAÇÃO DO CNES (<pasta_raiz>/dicionarios/cnes.duckdb)
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

# ==============================================================================
# CONFIGURAÇÃO DOS PARQUETS (IGUAL AO EXTRATOR: <pasta_app>/Tabelas_Todas/SINASC/FINAL)
# ==============================================================================
PATH_DADOS <- file.path(pasta_app, "Tabelas_Todas")

listar_parquets_sinasc <- function() {
  caminho_sinasc <- file.path(PATH_DADOS, "SINASC", "FINAL")
  if (dir.exists(caminho_sinasc)) {
    arquivos <- list.files(
      caminho_sinasc,
      pattern = "^FINAL_SINASC_.*\\.parquet$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    return(normalizePath(arquivos, winslash = "/", mustWork = FALSE))
  }
  return(character(0))
}

# Função para extrair metadados do nome do arquivo
extrair_metadados_parquet <- function(caminho_arquivo) {
  nome <- basename(caminho_arquivo)
  # Formato: FINAL_SINASC_UF_CIDADE_ANO.parquet
  # Exemplo: FINAL_SINASC_PR_Irati_2024.parquet
  
  nome_sem_ext <- gsub("\\.parquet$", "", nome)
  partes <- strsplit(nome_sem_ext, "_")[[1]]
  
  resultado <- list(
    caminho = caminho_arquivo,
    uf = NA_character_,
    cidade = NA_character_,
    ano = NA_integer_
  )
  
  if(length(partes) >= 5) {
    resultado$uf <- partes[3]
    resultado$cidade <- partes[4]
    
    ano_valor <- suppressWarnings(as.numeric(partes[5]))
    if(!is.na(ano_valor) && ano_valor > 1900 && ano_valor < 2100) {
      resultado$ano <- ano_valor
    }
  }
  
  return(resultado)
}

# ==============================================================================
# FUNÇÕES PARA OBTER ANOS E CIDADES DISPONÍVEIS
# ==============================================================================

# Função para obter cidades disponíveis
obter_cidades_disponiveis <- function() {
  arquivos <- listar_parquets_sinasc()

  arqs <- listar_parquets_sinasc()
message("Parquets encontrados: ", paste(arqs, collapse = " | "))
  
  if(length(arquivos) == 0) return(character(0))
  
  metadados <- purrr::map_df(arquivos, function(x) {
    meta <- extrair_metadados_parquet(x)
    data.frame(
      cidade = meta$cidade,
      stringsAsFactors = FALSE
    )
  })
  
  metadados <- metadados %>% filter(!is.na(cidade), cidade != "")
  cidades <- sort(unique(metadados$cidade))
  return(cidades)
}

# Função para obter anos disponíveis (filtra pela cidade selecionada)
obter_anos_disponiveis <- function(cidade_selecionada = NULL) {
  arquivos <- listar_parquets_sinasc()
  
  if(length(arquivos) == 0) return(integer(0))
  
  metadados <- purrr::map_df(arquivos, function(x) {
    meta <- extrair_metadados_parquet(x)
    data.frame(
      cidade = meta$cidade,
      ano = meta$ano,
      stringsAsFactors = FALSE
    )
  })
  
  metadados <- metadados %>% filter(!is.na(ano), !is.na(cidade), cidade != "")
  
  # Filtrar pela cidade selecionada
  if(!is.null(cidade_selecionada) && cidade_selecionada != "Todas" && cidade_selecionada != "") {
    metadados <- metadados %>% filter(tolower(cidade) == tolower(cidade_selecionada))
  }
  
  anos <- sort(unique(metadados$ano), decreasing = TRUE)
  return(anos)
}

# ==============================================================================
# FUNÇÃO PARA CARREGAR DADOS SINASC COM DUCKDB
# ==============================================================================

carregar_sinasc_parquet <- function(cidade_filtro = NULL, ano_filtro = NULL) {
  
  arquivos <- listar_parquets_sinasc()
  
  if(length(arquivos) == 0) {
    showNotification("Nenhum arquivo Parquet encontrado! Extraia dados primeiro.", type = "error")
    return(NULL)
  }
  
  # Extrair metadados de todos os arquivos
  metadados <- purrr::map_df(arquivos, function(x) {
    meta <- extrair_metadados_parquet(x)
    data.frame(
      caminho = meta$caminho,
      uf = meta$uf,
      cidade = meta$cidade,
      ano = meta$ano,
      stringsAsFactors = FALSE
    )
  })
  
  # Remover linhas com NA
  metadados <- metadados %>% filter(!is.na(ano), !is.na(cidade), cidade != "")
  
  # Aplicar filtros
  if(!is.null(cidade_filtro) && cidade_filtro != "Todas" && cidade_filtro != "") {
    metadados <- metadados %>% filter(tolower(cidade) == tolower(cidade_filtro))
  }
  
  if(!is.null(ano_filtro)) {
    metadados <- metadados %>% filter(ano == ano_filtro)
  }
  
  if(nrow(metadados) == 0) {
    showNotification(paste("Nenhum arquivo encontrado para os filtros selecionados."), type = "warning")
    return(NULL)
  }
  
  # ==============================================
  # LEITURA COM DUCKDB - UNIÃO DE TODOS OS PARQUETS
  # ==============================================
  
  # Construir a query SQL para ler múltiplos Parquets
  caminhos_arquivos <- metadados$caminho
  caminhos_sql <- paste0("'", caminhos_arquivos, "'", collapse = ", ")
  
  # Query para ler todos os Parquets de uma vez
  query <- sprintf("
    SELECT 
      *,
      '%s' as CIDADE_ORIGEM,
      '%s' as UF_ORIGEM
    FROM read_parquet([%s])
  ", metadados$cidade[1], metadados$uf[1], caminhos_sql)
  
  # Executar query no DuckDB
  df <- dbGetQuery(con, query)
  
  if(nrow(df) == 0) {
    return(NULL)
  }
  
  # Converter nomes das colunas para maiúsculo
  names(df) <- toupper(names(df))
  
  # Processar datas
  if("DTNASC" %in% names(df)) {
    df$DTNASC <- as.character(df$DTNASC)
    df$MES <- tryCatch({
      as.numeric(substr(df$DTNASC, 3, 4))
    }, error = function(e) {
      rep(1, nrow(df))
    })
  } else {
    df$MES <- 1
  }
  
  df$MES <- ifelse(is.na(df$MES) | df$MES < 1 | df$MES > 12, 1, df$MES)
  df$MES_NOME <- factor(df$MES, levels = 1:12, 
                        labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", 
                                   "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
  
  # Identificar colunas importantes
  col_estab <- intersect(names(df), c("CODESTAB", "ESTAB", "CNES"))[1]
  col_peso <- intersect(names(df), c("PESO", "PESO_NASC"))[1]
  col_idade_mae <- intersect(names(df), c("IDADEMAE", "IDADE_MAE"))[1]
  col_sexo <- intersect(names(df), c("SEXO", "SEXO_BEBE"))[1]
  col_apgar <- intersect(names(df), c("APGAR5", "APGAR_5"))[1]
  col_gestacao <- intersect(names(df), c("GESTACAO", "IDADE_GESTACIONAL"))[1]
  col_consultas <- intersect(names(df), c("CONSPRENAT", "CONSULTAS"))[1]
  col_mes_prenat <- intersect(names(df), c("MESPRENAT", "MES_PRENATAL"))[1]
  col_escolaridade <- intersect(names(df), c("ESCMAE", "ESCOLARIDADE"))[1]
  col_estcivil <- intersect(names(df), c("ESTCIVMAE", "ESTADO_CIVIL"))[1]
  col_parto <- intersect(names(df), c("PARTO", "TIPO_PARTO"))[1]
  col_sem_gest <- intersect(names(df), c("SEMAGESTAC", "SEMANAS_GESTACAO"))[1]
  col_mun_res <- intersect(names(df), c("CODMUNRES", "MUNICIPIO_RES"))[1]
  col_mun_nasc <- intersect(names(df), c("CODMUNNASC", "MUNICIPIO_NASC"))[1]
  
  # Converter colunas com segurança
  df$CODESTAB <- as.character(df[[col_estab]])
  df$PESO <- suppressWarnings(as.numeric(as.character(df[[col_peso]])))
  df$IDADEMAE <- suppressWarnings(as.numeric(as.character(df[[col_idade_mae]])))
  df$SEXO_VAL <- as.character(df[[col_sexo]])
  df$APGAR5 <- suppressWarnings(as.numeric(as.character(df[[col_apgar]])))
  df$GESTACAO <- suppressWarnings(as.numeric(as.character(df[[col_gestacao]])))
  df$CONSPRENAT <- suppressWarnings(as.numeric(as.character(df[[col_consultas]])))
  df$MESPRENAT <- suppressWarnings(as.numeric(as.character(df[[col_mes_prenat]])))
  df$SEMAGESTAC <- suppressWarnings(as.numeric(as.character(df[[col_sem_gest]])))
  df$PARTO_VAL <- as.character(df[[col_parto]])
  df$CODMUNRES <- as.character(df[[col_mun_res]])
  df$CODMUNNASC <- as.character(df[[col_mun_nasc]])
  
  # Classificações
  df$SEXO <- case_when(
    df$SEXO_VAL == "1" ~ "Masculino",
    df$SEXO_VAL == "2" ~ "Feminino",
    TRUE ~ "Ignorado"
  )
  
  df$PARTO_LABEL <- case_when(
    df$PARTO_VAL == "1" ~ "Vaginal",
    df$PARTO_VAL == "2" ~ "Cesáreo",
    TRUE ~ "Ignorado"
  )
  
  df$ESCOLARIDADE <- case_when(
    df[[col_escolaridade]] == "1" ~ "Sem instrução",
    df[[col_escolaridade]] == "2" ~ "1ª a 4ª série incompleta",
    df[[col_escolaridade]] == "3" ~ "4ª série completa",
    df[[col_escolaridade]] == "4" ~ "5ª a 8ª incompleta",
    df[[col_escolaridade]] == "5" ~ "Ensino fundamental completo",
    df[[col_escolaridade]] == "6" ~ "Ensino médio incompleto",
    df[[col_escolaridade]] == "7" ~ "Ensino médio completo",
    df[[col_escolaridade]] == "8" ~ "Superior incompleto",
    df[[col_escolaridade]] == "9" ~ "Superior completo",
    TRUE ~ "Ignorado"
  )
  
  df$ESTADO_CIVIL <- case_when(
    df[[col_estcivil]] == "1" ~ "Solteira",
    df[[col_estcivil]] == "2" ~ "Casada",
    df[[col_estcivil]] == "3" ~ "Viúva",
    df[[col_estcivil]] == "4" ~ "Separada/Divorciada",
    df[[col_estcivil]] == "5" ~ "União Estável",
    TRUE ~ "Ignorado"
  )
  
  df$Inicio_Precoce <- ifelse(df$MESPRENAT %in% c(1, 2, 3), "Sim", 
                              ifelse(is.na(df$MESPRENAT), "Ignorado", "Não"))
  
  df$Adequacao_Prenatal <- case_when(
    df$SEMAGESTAC < 37 & !is.na(df$SEMAGESTAC) & !is.na(df$CONSPRENAT) ~ {
      consultas_esperadas <- round((df$SEMAGESTAC / 40) * 6)
      ifelse(df$CONSPRENAT >= consultas_esperadas, "Adequado", "Inadequado")
    },
    df$SEMAGESTAC >= 37 & !is.na(df$SEMAGESTAC) & !is.na(df$CONSPRENAT) ~ {
      ifelse(df$CONSPRENAT >= 6, "Adequado", "Inadequado")
    },
    TRUE ~ "Ignorado"
  )
  
  df$FAIXA_APGAR <- case_when(
    df$APGAR5 >= 7 & !is.na(df$APGAR5) ~ "Normal (≥7)",
    df$APGAR5 >= 4 & df$APGAR5 <= 6 ~ "Asfixia moderada (4-6)",
    df$APGAR5 >= 0 & df$APGAR5 <= 3 ~ "Asfixia grave (<4)",
    TRUE ~ "Ignorado"
  )
  
  # Nome da cidade de residência e ocorrência
  df$CIDADE_RESIDENCIA <- sapply(df$CODMUNRES, function(cod) {
    if(is.null(cod) || is.na(cod) || cod == "") return(NA)
    cod <- str_pad(as.character(cod), 6, pad = "0")
    resultado <- municipios_df[municipios_df$id6 == cod, "nome"]
    if(length(resultado) > 0) return(resultado[1])
    return(cod)
  })
  
  df$CIDADE_OCORRENCIA <- sapply(df$CODMUNNASC, function(cod) {
    if(is.null(cod) || is.na(cod) || cod == "") return(NA)
    cod <- str_pad(as.character(cod), 6, pad = "0")
    resultado <- municipios_df[municipios_df$id6 == cod, "nome"]
    if(length(resultado) > 0) return(resultado[1])
    return(cod)
  })
  
  # Carregar CNES para coordenadas
  cnes_path <- "C:/Users/yurip.DESKTOP-LPNNGC7/OneDrive/Ambiente de Trabalho/cnes.duckdb"
  if(file.exists(cnes_path)) {
    con_cnes <- dbConnect(duckdb(), cnes_path, read_only = TRUE)
    cnes_completo <- dbGetQuery(con_cnes, "SELECT cnes, ibge, latitude, longitude, nome_estabelecimento FROM cnes")
    dbDisconnect(con_cnes, shutdown = TRUE)
    
    df <- df %>%
      left_join(cnes_completo, by = c("CODESTAB" = "cnes")) %>%
      mutate(
        ESTAB_NOME = ifelse(is.na(nome_estabelecimento), CODESTAB, nome_estabelecimento),
        latitude = ifelse(is.na(latitude), NA_real_, latitude),
        longitude = ifelse(is.na(longitude), NA_real_, longitude)
      )
  } else {
    df$ESTAB_NOME <- df$CODESTAB
    df$latitude <- NA_real_
    df$longitude <- NA_real_
  }
  
  # Tipo de fluxo baseado na cidade selecionada
  cidade_ref <- if(!is.null(cidade_filtro) && cidade_filtro != "Todas") cidade_filtro else NULL
  
  if(!is.null(cidade_ref)) {
    # Buscar código IBGE da cidade selecionada
    codigos <- municipios_df[tolower(municipios_df$nome) == tolower(cidade_ref), "id6"]
    codigo_ref <- as.character(codigos[1])
    
    if(is.na(codigo_ref) || length(codigo_ref) == 0) {
      codigo_ref <- "411070"
    }
    
    df$TIPO_FLUXO <- case_when(
      as.character(df$CODMUNRES) == codigo_ref & as.character(df$CODMUNNASC) == codigo_ref ~ paste("Reside e Nasce em", cidade_ref),
      as.character(df$CODMUNRES) == codigo_ref & as.character(df$CODMUNNASC) != codigo_ref ~ paste("Apenas Reside em", cidade_ref),
      as.character(df$CODMUNRES) != codigo_ref & as.character(df$CODMUNNASC) == codigo_ref ~ paste("Apenas Nasce em", cidade_ref),
      TRUE ~ "Outros"
    )
  } else {
    df$TIPO_FLUXO <- "Todas as cidades"
  }
  
  return(df)
}

options(scipen = 999)

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(
  dashboardHeader(title = uiOutput("titulo_dashboard")),
  
  dashboardSidebar(
    sidebarMenu(
      selectInput("cidade_sinasc", tags$b(icon("city"), " Cidade:"),
                  choices = NULL, selected = NULL),
      selectInput("ano_sinasc", "Selecione o Ano:", choices = NULL),
      hr(),
      menuItem("Visão Geral", tabName = "geral", icon = icon("tachometer-alt")),
      menuItem("Nascimentos", tabName = "nascimentos", icon = icon("baby")),
      menuItem("Perfil Materno", tabName = "materno", icon = icon("female")),
      menuItem("Pré-Natal", tabName = "prenatal", icon = icon("stethoscope")),
      menuItem("Fluxo de Pacientes", tabName = "fluxo", icon = icon("exchange-alt")),
      menuItem("Estabelecimentos", tabName = "estabelecimentos", icon = icon("hospital")),
      menuItem("🗺️ Mapa dos Nascimentos", tabName = "mapa", icon = icon("map")),
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
          valueBoxOutput("total_nascimentos", width = 3),
          valueBoxOutput("taxa_natalidade", width = 3),
          valueBoxOutput("total_estabelecimentos", width = 3),
          valueBoxOutput("taxa_cesarea", width = 3)
        ),
        fluidRow(
          valueBoxOutput("pre_natal_adequado", width = 3),
          valueBoxOutput("prematuridade", width = 3),
          valueBoxOutput("baixo_peso", width = 3),
          valueBoxOutput("mae_adolescente", width = 3)
        ),
        fluidRow(
          box(title = "Nascimentos por Mês", width = 12, status = "primary",
              plotlyOutput("evolucao_mensal", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Distribuição por Sexo", width = 6, status = "success",
              plotlyOutput("distribuicao_sexo", height = "400px")
          ),
          box(title = "Tipo de Parto (com meta OMS)", width = 6, status = "info",
              plotlyOutput("tipo_parto_meta", height = "400px")
          )
        )
      ),
      
      # ========== NASCIMENTOS ==========
      tabItem(
        tabName = "nascimentos",
        fluidRow(
          box(title = "Distribuição do Peso ao Nascer (com meta)", width = 12, status = "warning",
              plotlyOutput("distribuicao_peso_meta", height = "500px")
          )
        ),
        fluidRow(
          box(title = "APGAR no 5º Minuto (com meta)", width = 6, status = "warning",
              plotlyOutput("distribuicao_apgar_meta", height = "400px")
          ),
          box(title = "Idade Gestacional (com meta)", width = 6, status = "warning",
              plotlyOutput("idade_gestacional_meta", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Semana Gestacional x Peso", width = 12, status = "danger",
              plotlyOutput("scatter_peso_gestacao", height = "500px")
          )
        )
      ),
      
      # ========== PERFIL MATERNO ==========
      tabItem(
        tabName = "materno",
        fluidRow(
          box(title = "Idade da Mãe", width = 6, status = "primary",
              plotlyOutput("idade_mae", height = "400px")
          ),
          box(title = "Escolaridade da Mãe", width = 6, status = "primary",
              plotlyOutput("escolaridade_mae", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Estado Civil", width = 6, status = "info",
              plotlyOutput("estado_civil", height = "400px")
          ),
          box(title = "Número de Consultas Pré-Natal", width = 6, status = "info",
              plotlyOutput("consultas_prenatal", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Idade da Mãe x Escolaridade", width = 12, status = "warning",
              plotlyOutput("boxplot_idade_escolaridade", height = "500px")
          )
        )
      ),
      
      # ========== PRÉ-NATAL ==========
      tabItem(
        tabName = "prenatal",
        fluidRow(
          box(title = "Início do Pré-Natal (Mês da Gestação)", width = 12, status = "success",
              plotlyOutput("inicio_prenatal", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Adequação do Pré-Natal (com meta OMS)", width = 6, status = "success",
              plotlyOutput("adequacao_prenatal_meta", height = "400px")
          ),
          box(title = "Início Precoce (com meta)", width = 6, status = "success",
              plotlyOutput("inicio_precoce_meta", height = "400px")
          )
        ),
        fluidRow(
          box(title = "Adequação do Pré-Natal por Escolaridade", width = 12, status = "warning",
              plotlyOutput("adequacao_por_escolaridade", height = "500px")
          )
        )
      ),
      
      # ========== FLUXO DE PACIENTES ==========
      tabItem(
        tabName = "fluxo",
        fluidRow(
          box(title = "📊 Resumo do Fluxo de Pacientes", width = 12, status = "info",
              div(style = "font-size: 16px;", uiOutput("resumo_fluxo"))
          )
        ),
        fluidRow(
          box(title = "Fluxograma: Residência vs Ocorrência", width = 12, status = "primary",
              plotlyOutput("fluxo_sankey", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Cidades que mais enviam mães", width = 6, status = "warning",
              plotlyOutput("cidades_enviam", height = "500px")
          ),
          box(title = "Destinos das mães (quando não param na cidade selecionada)", width = 6, status = "warning",
              plotlyOutput("destinos_cidade", height = "500px")
          )
        ),
        fluidRow(
          box(title = "Detalhamento por Cidade de Residência", width = 12, status = "danger",
              DTOutput("tabela_detalhe_cidades")
          )
        ),
        fluidRow(
          box(title = "Evolução Temporal do Fluxo", width = 12, status = "info",
              plotlyOutput("evolucao_fluxo_mensal", height = "500px")
          )
        )
      ),
      
      # ========== ESTABELECIMENTOS ==========
      tabItem(
        tabName = "estabelecimentos",
        fluidRow(
          box(title = "Nascimentos por Estabelecimento", width = 12, status = "danger",
              plotlyOutput("nascimentos_estabelecimentos", height = "600px")
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
                        placeholder = "Ex: Santa Casa, Hospital"),
              br(),
              DTOutput("tabela_estab_busca")
          )
        )
      ),
      
      # ========== MAPA DOS NASCIMENTOS ==========
      tabItem(
        tabName = "mapa",
        fluidRow(
          box(
            title = "🗺️ Localização dos Nascimentos por Estabelecimento",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            div(
              style = "margin-bottom: 15px;",
              radioButtons("mapa_tipo", "Tipo de Visualização:",
                          choices = c(
                            "📍 Pontos por Estabelecimento" = "pontos",
                            "🔥 Heatmap (Densidade)" = "heatmap",
                            "🔵 Cluster (Agrupamento)" = "cluster"
                          ),
                          inline = TRUE
            )),
            leafletOutput("mapa_nascimentos", height = "600px")
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
          box(title = "Dados SINASC", width = 12, status = "primary",
              DTOutput("tabela_sinasc"),
              br(),
              downloadButton("download_sinasc", "📥 Baixar Dados (CSV)", class = "btn-success")
          )
        )
      ),
      
      # ========== RELATÓRIO ==========
      tabItem(
        tabName = "relatorio",
        fluidRow(
          box(title = "Relatório de Nascidos Vivos", width = 12, status = "success",
              downloadButton("download_relatorio", "📥 Baixar Relatório (HTML)", class = "btn-success"),
              br(), br(),
              htmlOutput("relatorio_sinasc")
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
  
  # ========== POPULAÇÃO (estimativa) ==========
  populacao <- reactive({
    return(60000)  # Estimativa para cidades médias do PR
  })
  
  # ========== CARREGAR CIDADES DISPONÍVEIS ==========
  observe({
    cidades <- obter_cidades_disponiveis()
    
    if(length(cidades) > 0) {
      opcoes <- c("Todas", cidades)
      updateSelectInput(session, "cidade_sinasc", choices = opcoes, selected = "Todas")
    } else {
      updateSelectInput(session, "cidade_sinasc", choices = c("Nenhum dado encontrado" = ""), selected = "")
      showNotification("Nenhum arquivo Parquet encontrado! Use o Extrator DATASUS primeiro.", 
                       type = "error", duration = 10)
    }
  })
  
  # ========== CARREGAR ANOS DISPONÍVEIS (dinâmico conforme cidade) ==========
  observeEvent(input$cidade_sinasc, {
    req(input$cidade_sinasc != "", input$cidade_sinasc != "Nenhum dado encontrado")
    
    cidade_param <- if(input$cidade_sinasc == "Todas") NULL else input$cidade_sinasc
    anos <- obter_anos_disponiveis(cidade_param)
    
    if(length(anos) > 0) {
      updateSelectInput(session, "ano_sinasc", choices = anos, selected = anos[1])
    } else {
      updateSelectInput(session, "ano_sinasc", choices = c("Nenhum ano encontrado" = ""), selected = "")
    }
  })
  
  # ========== TÍTULO DINÂMICO ==========
  output$titulo_dashboard <- renderUI({
    if(is.null(input$cidade_sinasc) || input$cidade_sinasc == "" || input$cidade_sinasc == "Todas") {
      paste("SINASC - Sistema de Informação sobre Nascidos Vivos - Todas as Cidades")
    } else {
      paste("SINASC - Sistema de Informação sobre Nascidos Vivos -", input$cidade_sinasc)
    }
  })
  
  # ========== CARREGAR DADOS SINASC ==========
  sinasc_data <- reactive({
    req(input$ano_sinasc, input$cidade_sinasc, input$cidade_sinasc != "", input$cidade_sinasc != "Nenhum dado encontrado")
    req(input$ano_sinasc != "Nenhum ano encontrado")
    
    cidade_param <- if(input$cidade_sinasc == "Todas") NULL else input$cidade_sinasc
    
    df <- carregar_sinasc_parquet(
      cidade_filtro = cidade_param,
      ano_filtro = as.numeric(input$ano_sinasc)
    )
    
    if(is.null(df)) {
      showNotification("Nenhum dado encontrado para os filtros selecionados.", type = "warning")
      return(NULL)
    }
    
    return(df)
  })
  
  # ========== DADOS PARA MAPA ==========
  dados_mapa <- reactive({
    req(sinasc_data())
    
    df <- sinasc_data() %>%
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      group_by(CODESTAB, ESTAB_NOME, latitude, longitude) %>%
      summarise(
        total_nascimentos = n(),
        parto_vaginal = sum(PARTO_VAL == "1", na.rm = TRUE),
        parto_cesareo = sum(PARTO_VAL == "2", na.rm = TRUE),
        peso_medio = round(mean(PESO, na.rm = TRUE), 0),
        apgar_medio = round(mean(APGAR5, na.rm = TRUE), 1),
        mae_adolescente = sum(IDADEMAE < 20, na.rm = TRUE),
        mae_mais_35 = sum(IDADEMAE >= 35, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        perc_cesareo = round(parto_cesareo / total_nascimentos * 100, 1),
        popup_text = paste0(
          "<b>", ESTAB_NOME, "</b><br>",
          "📍 Total de nascimentos: ", total_nascimentos, "<br>",
          "🚼 Partos vaginais: ", parto_vaginal, "<br>",
          "🔪 Partos cesáreos: ", parto_cesareo, " (", perc_cesareo, "%)<br>",
          "⚖️ Peso médio: ", peso_medio, "g<br>",
          "💚 APGAR médio: ", apgar_medio, "<br>",
          "👧 Mães adolescentes: ", mae_adolescente, "<br>",
          "👩 Mães com 35+ anos: ", mae_mais_35
        )
      )
    
    return(df)
  })
  
  # ========== MAPA ==========
  output$mapa_nascimentos <- renderLeaflet({
    req(dados_mapa())
    
    df <- dados_mapa()
    
    pal <- colorNumeric(
      palette = "RdYlBu",
      domain = df$total_nascimentos,
      reverse = TRUE
    )
    
    map <- leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -50.65, lat = -25.47, zoom = 12)
    
    if(input$mapa_tipo == "pontos") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = ~sqrt(total_nascimentos) * 1.5,
          color = ~pal(total_nascimentos),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2,
          popup = ~popup_text,
          label = ~ESTAB_NOME,
          group = "Estabelecimentos"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~total_nascimentos,
          title = "Total de Nascimentos",
          opacity = 0.7
        )
      
    } else if(input$mapa_tipo == "heatmap") {
      map <- map %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          intensity = ~total_nascimentos,
          blur = 20,
          max = 0.05,
          radius = 15,
          group = "Heatmap"
        ) %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 6,
          color = "black",
          fillOpacity = 0.3,
          stroke = TRUE,
          weight = 1,
          popup = ~popup_text,
          label = ~ESTAB_NOME,
          group = "Marcadores"
        )
      
    } else if(input$mapa_tipo == "cluster") {
      map <- map %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 8,
          color = "#e74c3c",
          fillOpacity = 0.6,
          popup = ~popup_text,
          label = ~ESTAB_NOME,
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
  
  # ========== TABELA ESTABELECIMENTOS MAPA ==========
  output$tabela_estabelecimentos_mapa <- renderDT({
    req(dados_mapa())
    
    df <- dados_mapa() %>%
      select(
        ESTAB_NOME, total_nascimentos, parto_vaginal, parto_cesareo,
        perc_cesareo, peso_medio, apgar_medio, mae_adolescente, mae_mais_35
      ) %>%
      arrange(desc(total_nascimentos))
    
    datatable(
      df,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
      ),
      colnames = c(
        "Estabelecimento", "Total", "Parto Vaginal", "Parto Cesáreo",
        "% Cesáreo", "Peso Médio (g)", "APGAR Médio", "Mães <20", "Mães ≥35"
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "peso_medio", digits = 0) %>%
      formatRound(columns = "apgar_medio", digits = 1) %>%
      formatPercentage(columns = "perc_cesareo", digits = 1)
  })
  
  # ========== VALUE BOXES ==========
  
  output$total_nascimentos <- renderValueBox({
    req(sinasc_data())
    cidade_nome <- if(is.null(input$cidade_sinasc) || input$cidade_sinasc == "Todas") "Todas as Cidades" else input$cidade_sinasc
    valueBox(
      value = format(nrow(sinasc_data()), big.mark = "."),
      subtitle = paste("Total de Nascimentos -", cidade_nome),
      icon = icon("baby"),
      color = "blue"
    )
  })
  
  output$taxa_natalidade <- renderValueBox({
    req(sinasc_data(), populacao())
    taxa <- round(nrow(sinasc_data()) / populacao() * 1000, 2)
    valueBox(
      value = paste(taxa, "/1.000 hab"),
      subtitle = "Taxa de Natalidade (estimada)",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$total_estabelecimentos <- renderValueBox({
    req(sinasc_data())
    total <- n_distinct(sinasc_data()$CODESTAB, na.rm = TRUE)
    valueBox(
      value = total,
      subtitle = "Estabelecimentos",
      icon = icon("hospital"),
      color = "yellow"
    )
  })
  
  output$taxa_cesarea <- renderValueBox({
    req(sinasc_data())
    df <- sinasc_data()
    taxa <- round(sum(df$PARTO_VAL == "2", na.rm = TRUE) / nrow(df) * 100, 1)
    color <- ifelse(taxa <= 15, "green", ifelse(taxa <= 20, "yellow", "red"))
    valueBox(
      value = paste(taxa, "%"),
      subtitle = "Taxa de Cesárea",
      icon = icon("cut"),
      color = color
    )
  })
  
  output$pre_natal_adequado <- renderValueBox({
    req(sinasc_data())
    df <- sinasc_data()
    adequado <- sum(df$Adequacao_Prenatal == "Adequado", na.rm = TRUE)
    total_valido <- sum(df$Adequacao_Prenatal %in% c("Adequado", "Inadequado"), na.rm = TRUE)
    perc <- ifelse(total_valido > 0, round(adequado / total_valido * 100, 1), 0)
    color <- ifelse(perc >= 95, "green", ifelse(perc >= 80, "yellow", "red"))
    valueBox(
      value = paste(perc, "%"),
      subtitle = "Pré-Natal Adequado (Meta ≥95%)",
      icon = icon("stethoscope"),
      color = color
    )
  })
  
  output$prematuridade <- renderValueBox({
    req(sinasc_data())
    df <- sinasc_data()
    perc <- round(sum(df$GESTACAO < 4, na.rm = TRUE) / nrow(df) * 100, 1)
    color <- ifelse(perc <= 10, "green", ifelse(perc <= 15, "yellow", "red"))
    valueBox(
      value = paste(perc, "%"),
      subtitle = "Prematuridade (Meta ≤10%)",
      icon = icon("clock"),
      color = color
    )
  })
  
  output$baixo_peso <- renderValueBox({
    req(sinasc_data())
    df <- sinasc_data()
    perc <- round(sum(df$PESO < 2500, na.rm = TRUE) / nrow(df) * 100, 1)
    color <- ifelse(perc <= 8, "green", ifelse(perc <= 10, "yellow", "red"))
    valueBox(
      value = paste(perc, "%"),
      subtitle = "Baixo Peso (Meta ≤8%)",
      icon = icon("weight"),
      color = color
    )
  })
  
  output$mae_adolescente <- renderValueBox({
    req(sinasc_data())
    df <- sinasc_data()
    perc <- round(sum(df$IDADEMAE < 20, na.rm = TRUE) / nrow(df) * 100, 1)
    color <- ifelse(perc <= 10, "green", ifelse(perc <= 15, "yellow", "red"))
    valueBox(
      value = paste(perc, "%"),
      subtitle = "Mães Adolescentes (Meta ≤10%)",
      icon = icon("heart"),
      color = color
    )
  })
  
  # ========== GRÁFICOS PRINCIPAIS ==========
  
  output$evolucao_mensal <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      group_by(MES, MES_NOME) %>%
      summarise(Total = n(), .groups = 'drop')
    
    plot_ly(df, x = ~MES_NOME, y = ~Total, type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 2),
            marker = list(color = "#2980b9", size = 8)) %>%
      layout(title = paste("Nascimentos por Mês -", input$ano_sinasc),
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Nascimentos"))
  })
  
  output$distribuicao_sexo <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      group_by(SEXO) %>%
      summarise(Total = n(), .groups = 'drop')
    
    plot_ly(df, labels = ~SEXO, values = ~Total, type = "pie",
            marker = list(colors = c("#3498db", "#e74c3c", "#95a5a6"))) %>%
      layout(title = "Distribuição por Sexo")
  })
  
  output$tipo_parto_meta <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(PARTO_LABEL != "Ignorado") %>%
      group_by(PARTO_LABEL) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      mutate(Percentual = Total / sum(Total) * 100)
    
    meta_cesarea <- 15
    plot_ly() %>%
      add_trace(data = df, x = ~PARTO_LABEL, y = ~Percentual, type = "bar",
                marker = list(color = c("#2ecc71", "#e74c3c")),
                name = "Observado") %>%
      add_trace(x = c("Cesáreo", "Cesáreo"), y = c(meta_cesarea, meta_cesarea), 
                type = "scatter", mode = "lines",
                line = list(color = "red", dash = "dash", width = 2),
                name = "Meta OMS (15%)") %>%
      layout(title = "Tipo de Parto - Comparação com Meta OMS",
             xaxis = list(title = ""),
             yaxis = list(title = "Percentual (%)", range = c(0, max(df$Percentual, meta_cesarea) + 10)))
  })
  
  output$distribuicao_peso_meta <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>% filter(!is.na(PESO) & PESO > 0 & PESO < 6000)
    
    meta_baixo_peso <- 8
    perc_baixo_peso <- round(sum(df$PESO < 2500, na.rm = TRUE) / nrow(df) * 100, 1)
    
    plot_ly() %>%
      add_trace(data = df, x = ~PESO, type = "histogram", nbinsx = 40,
                marker = list(color = "#9b59b6", line = list(color = "white", width = 0.5)),
                name = "Nascimentos") %>%
      add_trace(x = c(2500, 2500), y = c(0, max(hist(df$PESO, plot = FALSE)$counts) * 1.1),
                type = "scatter", mode = "lines",
                line = list(color = "red", dash = "dash", width = 2),
                name = "Baixo peso (<2500g)") %>%
      add_annotations(x = 2600, y = max(hist(df$PESO, plot = FALSE)$counts) * 0.9,
                      text = paste("Baixo peso:", perc_baixo_peso, "% (Meta: ≤8%)"),
                      showarrow = FALSE, font = list(color = "red", size = 12)) %>%
      layout(title = "Distribuição do Peso ao Nascer",
             xaxis = list(title = "Peso (gramas)"),
             yaxis = list(title = "Frequência"))
  })
  
  output$distribuicao_apgar_meta <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(FAIXA_APGAR != "Ignorado") %>%
      group_by(FAIXA_APGAR) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      mutate(Percentual = Total / sum(Total) * 100)
    
    meta_apgar <- 95
    plot_ly() %>%
      add_trace(data = df, x = ~FAIXA_APGAR, y = ~Percentual, type = "bar",
                marker = list(color = c("#2ecc71", "#f39c12", "#e74c3c")),
                name = "Observado") %>%
      add_trace(x = c("Normal (≥7)", "Normal (≥7)"), y = c(meta_apgar, meta_apgar),
                type = "scatter", mode = "lines",
                line = list(color = "red", dash = "dash", width = 2),
                name = "Meta (≥95%)") %>%
      layout(title = "APGAR no 5º Minuto",
             xaxis = list(title = ""),
             yaxis = list(title = "Percentual (%)", range = c(0, 100)))
  })
  
  output$idade_gestacional_meta <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(!is.na(GESTACAO)) %>%
      mutate(CATEGORIA = case_when(
        GESTACAO == 1 ~ "Pré-termo (<37s)",
        GESTACAO == 2 ~ "Termo (37-41s)",
        TRUE ~ "Pós-termo (42s+)"
      )) %>%
      group_by(CATEGORIA) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      mutate(Percentual = Total / sum(Total) * 100)
    
    meta_prematuro <- 10
    plot_ly() %>%
      add_trace(data = df, x = ~CATEGORIA, y = ~Percentual, type = "bar",
                marker = list(color = c("#e74c3c", "#2ecc71", "#f39c12")),
                name = "Observado") %>%
      add_trace(x = c("Pré-termo (<37s)", "Pré-termo (<37s)"), y = c(meta_prematuro, meta_prematuro),
                type = "scatter", mode = "lines",
                line = list(color = "red", dash = "dash", width = 2),
                name = "Meta (≤10%)") %>%
      layout(title = "Idade Gestacional",
             xaxis = list(title = ""),
             yaxis = list(title = "Percentual (%)", range = c(0, max(df$Percentual, meta_prematuro) + 10)))
  })
  
  output$scatter_peso_gestacao <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(!is.na(PESO) & !is.na(SEMAGESTAC) & PESO > 0 & SEMAGESTAC > 0)
    
    plot_ly(df, x = ~SEMAGESTAC, y = ~PESO, type = "scatter", mode = "markers",
            marker = list(color = ~PESO, size = 5, showscale = TRUE,
                         colorscale = "Viridis"),
            text = ~paste("Semana:", SEMAGESTAC, "<br>Peso:", PESO, "g"),
            hoverinfo = "text") %>%
      layout(title = "Relação entre Semana Gestacional e Peso ao Nascer",
             xaxis = list(title = "Semana Gestacional"),
             yaxis = list(title = "Peso (gramas)"))
  })
  
  output$idade_mae <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>% filter(!is.na(IDADEMAE) & IDADEMAE > 0 & IDADEMAE < 100)
    
    plot_ly(df, x = ~IDADEMAE, type = "histogram", nbinsx = 20,
            marker = list(color = "#e67e22")) %>%
      layout(title = "Distribuição da Idade das Mães",
             xaxis = list(title = "Idade (anos)"),
             yaxis = list(title = "Frequência"))
  })
  
  output$escolaridade_mae <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(ESCOLARIDADE != "Ignorado") %>%
      group_by(ESCOLARIDADE) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total))
    
    plot_ly(df, x = ~Total, y = ~reorder(ESCOLARIDADE, Total), type = "bar", orientation = "h",
            marker = list(color = "#3498db")) %>%
      layout(title = "Escolaridade das Mães",
             xaxis = list(title = "Nascimentos"),
             yaxis = list(title = ""))
  })
  
  output$estado_civil <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(ESTADO_CIVIL != "Ignorado") %>%
      group_by(ESTADO_CIVIL) %>%
      summarise(Total = n(), .groups = 'drop')
    
    plot_ly(df, labels = ~ESTADO_CIVIL, values = ~Total, type = "pie") %>%
      layout(title = "Estado Civil das Mães")
  })
  
  output$consultas_prenatal <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>% filter(!is.na(CONSPRENAT) & CONSPRENAT <= 20)
    
    plot_ly(df, x = ~CONSPRENAT, type = "histogram", nbinsx = 20,
            marker = list(color = "#2ecc71")) %>%
      layout(title = "Número de Consultas Pré-Natal",
             xaxis = list(title = "Consultas"),
             yaxis = list(title = "Frequência"))
  })
  
  output$boxplot_idade_escolaridade <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(!is.na(IDADEMAE) & ESCOLARIDADE != "Ignorado" & IDADEMAE > 0)
    
    plot_ly(df, x = ~ESCOLARIDADE, y = ~IDADEMAE, type = "box",
            marker = list(color = "#9b59b6")) %>%
      layout(title = "Idade da Mãe por Escolaridade",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Idade (anos)"))
  })
  
  output$inicio_prenatal <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(!is.na(MESPRENAT) & MESPRENAT >= 0 & MESPRENAT <= 12) %>%
      group_by(MESPRENAT) %>%
      summarise(Total = n(), .groups = 'drop')
    
    plot_ly(df, x = ~MESPRENAT, y = ~Total, type = "bar",
            marker = list(color = "#1abc9c")) %>%
      layout(title = "Mês de Gestação de Início do Pré-Natal",
             xaxis = list(title = "Mês da Gestação", dtick = 1),
             yaxis = list(title = "Nascimentos"))
  })
  
  output$adequacao_prenatal_meta <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(Adequacao_Prenatal != "Ignorado") %>%
      group_by(Adequacao_Prenatal) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      mutate(Percentual = Total / sum(Total) * 100)
    
    meta_adequado <- 95
    plot_ly() %>%
      add_trace(data = df, x = ~Adequacao_Prenatal, y = ~Percentual, type = "bar",
                marker = list(color = c("#2ecc71", "#e74c3c")),
                name = "Observado") %>%
      add_trace(x = c("Adequado", "Adequado"), y = c(meta_adequado, meta_adequado),
                type = "scatter", mode = "lines",
                line = list(color = "red", dash = "dash", width = 2),
                name = "Meta (≥95%)") %>%
      layout(title = "Adequação do Pré-Natal (Ajustado por IG)",
             xaxis = list(title = ""),
             yaxis = list(title = "Percentual (%)", range = c(0, 100)))
  })
  
  output$inicio_precoce_meta <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(Inicio_Precoce != "Ignorado") %>%
      group_by(Inicio_Precoce) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      mutate(Percentual = Total / sum(Total) * 100)
    
    meta_inicio <- 80
    plot_ly() %>%
      add_trace(data = df, x = ~Inicio_Precoce, y = ~Percentual, type = "bar",
                marker = list(color = c("#2ecc71", "#e74c3c")),
                name = "Observado") %>%
      add_trace(x = c("Sim", "Sim"), y = c(meta_inicio, meta_inicio),
                type = "scatter", mode = "lines",
                line = list(color = "red", dash = "dash", width = 2),
                name = "Meta (≥80%)") %>%
      layout(title = "Início do Pré-Natal no 1º Trimestre",
             xaxis = list(title = ""),
             yaxis = list(title = "Percentual (%)", range = c(0, 100)))
  })
  
  output$adequacao_por_escolaridade <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      filter(Adequacao_Prenatal != "Ignorado", ESCOLARIDADE != "Ignorado") %>%
      group_by(ESCOLARIDADE, Adequacao_Prenatal) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      group_by(ESCOLARIDADE) %>%
      mutate(Percentual = Total / sum(Total) * 100)
    
    plot_ly(df, x = ~ESCOLARIDADE, y = ~Percentual, color = ~Adequacao_Prenatal,
            type = "bar", colors = c("#2ecc71", "#e74c3c")) %>%
      layout(title = "Adequação do Pré-Natal por Escolaridade",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Percentual (%)"),
             barmode = "stack")
  })
  
  # ========== FLUXO DE PACIENTES ==========
  
  output$resumo_fluxo <- renderUI({
    req(sinasc_data())
    req(input$cidade_sinasc != "Todas")
    
    df <- sinasc_data()
    cidade_ref <- input$cidade_sinasc
    
    total <- nrow(df)
    reside_nasce <- sum(grepl("Reside e Nasce", df$TIPO_FLUXO), na.rm = TRUE)
    apenas_reside <- sum(grepl("Apenas Reside", df$TIPO_FLUXO), na.rm = TRUE)
    apenas_nasce <- sum(grepl("Apenas Nasce", df$TIPO_FLUXO), na.rm = TRUE)
    
    perc_reside_nasce <- if(total > 0) round(reside_nasce / total * 100, 1) else 0
    perc_apenas_reside <- if(total > 0) round(apenas_reside / total * 100, 1) else 0
    perc_apenas_nasce <- if(total > 0) round(apenas_nasce / total * 100, 1) else 0
    
    saldo <- apenas_nasce - apenas_reside
    
    div(
      style = "display: flex; justify-content: space-around; text-align: center; flex-wrap: wrap;",
      div(style = "background: #2ecc71; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(reside_nasce, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Residem e Nascem em", cidade_ref)),
          p(style = "margin: 0; color: white; font-size: 12px;", paste0("(", perc_reside_nasce, "%)"))
      ),
      div(style = "background: #f39c12; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(apenas_reside, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Apenas Residem em", cidade_ref)),
          p(style = "margin: 0; color: white; font-size: 12px;", paste0("(Pariram fora - ", perc_apenas_reside, "%)"))
      ),
      div(style = "background: #e74c3c; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", format(apenas_nasce, big.mark = ".")),
          p(style = "margin: 0; color: white;", paste("Apenas Nascem em", cidade_ref)),
          p(style = "margin: 0; color: white; font-size: 12px;", paste0("(Mães de fora - ", perc_apenas_nasce, "%)"))
      ),
      div(style = "background: #3498db; padding: 20px; border-radius: 10px; margin: 10px; min-width: 150px;",
          h3(style = "margin: 0; color: white;", paste0(saldo >= 0 ? "+" : "", saldo)),
          p(style = "margin: 0; color: white;", "Saldo (Pacientes de fora)"),
          p(style = "margin: 0; color: white; font-size: 12px;", ifelse(saldo > 0, "Atende mais do que perde", ifelse(saldo < 0, "Perde pacientes", "Neutro")))
      )
    )
  })
  
  output$fluxo_sankey <- renderPlotly({
    req(sinasc_data())
    req(input$cidade_sinasc != "Todas")
    
    df <- sinasc_data()
    cidade_ref <- input$cidade_sinasc
    
    reside <- ifelse(df$TIPO_FLUXO %in% c(paste("Reside e Nasce em", cidade_ref), paste("Apenas Reside em", cidade_ref)), 
                     paste("Reside em", cidade_ref), "Reside em outros")
    nasce <- ifelse(df$TIPO_FLUXO %in% c(paste("Reside e Nasce em", cidade_ref), paste("Apenas Nasce em", cidade_ref)),
                    paste("Nasce em", cidade_ref), "Nasce em outros")
    
    fonte <- c(0, 0, 1, 1)
    alvo <- c(2, 3, 2, 3)
    valores <- c(
      sum(reside == paste("Reside em", cidade_ref) & nasce == paste("Nasce em", cidade_ref), na.rm = TRUE),
      sum(reside == paste("Reside em", cidade_ref) & nasce != paste("Nasce em", cidade_ref), na.rm = TRUE),
      sum(reside != paste("Reside em", cidade_ref) & nasce == paste("Nasce em", cidade_ref), na.rm = TRUE),
      sum(reside != paste("Reside em", cidade_ref) & nasce != paste("Nasce em", cidade_ref), na.rm = TRUE)
    )
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = c(paste("Reside em", cidade_ref), "Reside em outros", paste("Nasce em", cidade_ref), "Nasce em outros"),
        color = c("#2ecc71", "#e74c3c", "#2ecc71", "#e74c3c"),
        pad = 15,
        thickness = 20
      ),
      link = list(
        source = fonte,
        target = alvo,
        value = valores
      )
    ) %>%
      layout(title = paste("Fluxo de Pacientes:", cidade_ref),
             font = list(size = 12))
  })
  
  output$cidades_enviam <- renderPlotly({
    req(sinasc_data())
    req(input$cidade_sinasc != "Todas")
    
    cidade_ref <- input$cidade_sinasc
    
    df <- sinasc_data() %>%
      filter(TIPO_FLUXO == paste("Apenas Nasce em", cidade_ref)) %>%
      group_by(CODMUNRES, CIDADE_RESIDENCIA) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      filter(!is.na(CIDADE_RESIDENCIA)) %>%
      arrange(desc(Total)) %>%
      head(10)
    
    if(nrow(df) == 0) {
      plot_ly() %>% layout(title = "Nenhuma cidade enviou mães")
    } else {
      plot_ly(df, x = ~Total, y = ~reorder(CIDADE_RESIDENCIA, Total), type = "bar", orientation = "h",
              marker = list(color = "#f39c12")) %>%
        layout(title = paste("Cidades que mais enviam mães para", cidade_ref),
               xaxis = list(title = "Número de Mães"),
               yaxis = list(title = ""))
    }
  })
  
  output$destinos_cidade <- renderPlotly({
    req(sinasc_data())
    req(input$cidade_sinasc != "Todas")
    
    cidade_ref <- input$cidade_sinasc
    
    df <- sinasc_data() %>%
      filter(TIPO_FLUXO == paste("Apenas Reside em", cidade_ref)) %>%
      group_by(CODMUNNASC, CIDADE_OCORRENCIA) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      filter(!is.na(CIDADE_OCORRENCIA)) %>%
      arrange(desc(Total)) %>%
      head(10)
    
    if(nrow(df) == 0) {
      plot_ly() %>% layout(title = "Nenhuma mãe saiu da cidade")
    } else {
      plot_ly(df, x = ~Total, y = ~reorder(CIDADE_OCORRENCIA, Total), type = "bar", orientation = "h",
              marker = list(color = "#e74c3c")) %>%
        layout(title = paste("Destinos das mães de", cidade_ref),
               xaxis = list(title = "Número de Mães"),
               yaxis = list(title = ""))
    }
  })
  
  output$tabela_detalhe_cidades <- renderDT({
    req(sinasc_data())
    
    df_resumo <- sinasc_data() %>%
      filter(TIPO_FLUXO != "Todas as cidades") %>%
      group_by(CIDADE_RESIDENCIA, TIPO_FLUXO) %>%
      summarise(Quantidade = n(), .groups = 'drop') %>%
      arrange(desc(Quantidade))
    
    datatable(df_resumo, 
              options = list(scrollX = TRUE, pageLength = 15),
              colnames = c("Cidade de Residência", "Tipo de Fluxo", "Quantidade"),
              caption = "Detalhamento por cidade de residência")
  })
  
  output$evolucao_fluxo_mensal <- renderPlotly({
    req(sinasc_data())
    
    df <- sinasc_data() %>%
      filter(TIPO_FLUXO != "Todas as cidades") %>%
      group_by(MES_NOME, TIPO_FLUXO) %>%
      summarise(Total = n(), .groups = 'drop')
    
    plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~TIPO_FLUXO, 
            type = "scatter", mode = "lines+markers") %>%
      layout(title = "Evolução Mensal do Fluxo de Pacientes",
             xaxis = list(title = "Mês"),
             yaxis = list(title = "Nascimentos"))
  })
  
  # ========== ESTABELECIMENTOS ==========
  
  output$nascimentos_estabelecimentos <- renderPlotly({
    req(sinasc_data())
    df <- sinasc_data() %>%
      group_by(CODESTAB, ESTAB_NOME) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      arrange(desc(Total)) %>%
      head(15)
    
    plot_ly(df, x = ~Total, y = ~reorder(ESTAB_NOME, Total), type = "bar", orientation = "h",
            marker = list(color = "#f39c12")) %>%
      layout(title = "Nascimentos por Estabelecimento",
             xaxis = list(title = "Nascimentos"),
             yaxis = list(title = ""))
  })
  
  output$comparativo_estabelecimentos <- renderPlotly({
    req(sinasc_data())
    top5 <- sinasc_data() %>%
      group_by(CODESTAB) %>%
      summarise(Total = n(), .groups = 'drop') %>%
      top_n(5, Total) %>%
      pull(CODESTAB)
    
    if(length(top5) > 0) {
      df <- sinasc_data() %>%
        filter(CODESTAB %in% top5) %>%
        group_by(ESTAB_NOME, MES_NOME) %>%
        summarise(Total = n(), .groups = 'drop')
      
      plot_ly(df, x = ~MES_NOME, y = ~Total, color = ~ESTAB_NOME, 
              type = "scatter", mode = "lines+markers") %>%
        layout(title = "Comparativo Mensal - Top 5 Estabelecimentos",
               xaxis = list(title = "Mês"),
               yaxis = list(title = "Nascimentos"))
    } else {
      plot_ly() %>% layout(title = "Sem dados suficientes para comparativo")
    }
  })
  
  output$tabela_estab_busca <- renderDT({
    req(sinasc_data(), input$buscar_estabelecimento)
    
    if(input$buscar_estabelecimento != "") {
      busca <- toupper(input$buscar_estabelecimento)
      df <- sinasc_data() %>%
        filter(grepl(busca, toupper(ESTAB_NOME), ignore.case = TRUE)) %>%
        group_by(CODESTAB, ESTAB_NOME) %>%
        summarise(Total = n(), .groups = 'drop') %>%
        arrange(desc(Total))
      
      datatable(df, options = list(pageLength = 10), rownames = FALSE,
                caption = paste("Resultados para:", input$buscar_estabelecimento))
    } else {
      datatable(data.frame(Mensagem = "Digite um nome para buscar"), rownames = FALSE)
    }
  })
  
  # ========== DADOS BRUTOS ==========
  
  output$tabela_sinasc <- renderDT({
    req(sinasc_data())
    datatable(sinasc_data() %>% 
                select(CODESTAB, ESTAB_NOME, DTNASC, SEXO, PESO, IDADEMAE, 
                       ESCOLARIDADE, CONSPRENAT, PARTO_LABEL, CIDADE_RESIDENCIA, MES_NOME) %>% 
                head(500),
              options = list(scrollX = TRUE, pageLength = 20),
              colnames = c("CNES", "Estabelecimento", "Data", "Sexo", "Peso(g)", "Idade Mãe", 
                          "Escolaridade", "Consultas PN", "Tipo Parto", "Cidade Residência", "Mês"))
  })
  
  output$download_sinasc <- downloadHandler(
    filename = function() paste0("sinasc_", input$cidade_sinasc, "_", input$ano_sinasc, "_", Sys.Date(), ".csv"),
    content = function(file) {
      req(sinasc_data())
      write.csv(sinasc_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$relatorio_sinasc <- renderUI({
    req(sinasc_data())
    
    cidade_nome <- if(input$cidade_sinasc == "Todas") "Todas as Cidades" else input$cidade_sinasc
    total <- nrow(sinasc_data())
    taxa_cesarea <- round(sum(sinasc_data()$PARTO_VAL == "2", na.rm = TRUE) / total * 100, 1)
    
    HTML(paste0("
    <div style='font-family: Arial; padding: 20px;'>
      <h1 style='color: #3498db;'>RELATÓRIO DE NASCIDOS VIVOS</h1>
      <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sinasc, "</h3>
      <hr/>
      <p>Dados extraídos de arquivos Parquet gerados pelo Extrator DATASUS.</p>
      <h2>Resumo</h2>
      <ul>
        <li>Total de Nascimentos: <strong>", format(total, big.mark = "."), "</strong></li>
        <li>Taxa de Cesárea: <strong>", taxa_cesarea, "%</strong></li>
      </ul>
      <hr/>
      <p>Relatório gerado em ", Sys.Date(), "</p>
    </div>
    "))
  })
  
  output$download_relatorio <- downloadHandler(
    filename = function() paste0("relatorio_sinasc_", input$cidade_sinasc, "_", input$ano_sinasc, "_", Sys.Date(), ".html"),
    content = function(file) {
      req(sinasc_data())
      cidade_nome <- if(input$cidade_sinasc != "Todas") input$cidade_sinasc else "Todas"
      total <- nrow(sinasc_data())
      
      html <- paste0("
      <html>
      <head><style>body{font-family:Arial;margin:40px;}</style></head>
      <body>
        <h1>RELATÓRIO DE NASCIDOS VIVOS</h1>
        <h3>Cidade: ", cidade_nome, " | Ano: ", input$ano_sinasc, "</h3>
        <hr/>
        <h2>Resumo</h2>
        <p>Total de Nascimentos: ", format(total, big.mark = "."), "</p>
        <hr/>
        <p>Gerado em ", Sys.Date(), "</p>
      </body>
      </html>
      ")
      writeLines(html, file)
    }
  )
  
}

# ==============================================================================
# EXECUTAR
# ==============================================================================
shinyApp(ui = ui, server = server)

