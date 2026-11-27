<div align="center">

# 📊 Extratores DATASUS + IBGE

**Suíte de aplicativos standalone para extração, filtragem e armazenamento de microdados de saúde pública e dados territoriais do Brasil — tudo em formato Parquet.**

[![R](https://img.shields.io/badge/R-4.0%2B-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://cran.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Apps-1e90ff?style=for-the-badge&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)
[![Parquet](https://img.shields.io/badge/Apache_Parquet-Colunar-50ABF1?style=for-the-badge&logo=apache&logoColor=white)](https://parquet.apache.org/)
[![License](https://img.shields.io/badge/License-MIT-green?style=for-the-badge)](LICENSE)
[![Status](https://img.shields.io/badge/Status-Ativo-success?style=for-the-badge)]()

[🎯 O que é](#-o-que-é) · [✨ Diferenciais](#-diferenciais) · [🚀 Como começar](#-como-começar) · [📁 Estrutura](#-estrutura) · [🩺 Sistemas](#-sistemas) · [🔧 Pipeline](#-pipeline-técnico) · [🗺️ Roadmap](#️-roadmap) · [❓ FAQ](#-faq)

</div>

---

## 🎯 O que é

Conjunto de **5 aplicativos Shiny independentes** que baixam, filtram e armazenam dados de saúde e territoriais do Brasil. Cada app:

- 🖱️ **Abre com duplo clique** (sem precisar saber R)
- 📦 **Instala dependências automaticamente** na primeira execução
- 📖 **Tem manual embutido** dentro do próprio app
- 💾 **Salva em Parquet** — formato colunar, comprimido, compatível com Python/DuckDB/Power BI
- 🔗 **Compartilha a mesma estrutura de pastas** — permitindo cruzar sistemas por código IBGE

| App | Fonte | Recorte | Unidade temporal | Sistema |
|---|---|---|---|---|
| 🩺 **SIA** | DATASUS | Ambulatorial | Mensal | SIA-PA · SIA-BI · SIA-AQ · SIA-AN |
| 🏥 **SIH** | DATASUS | Internações | Mensal | SIH-RD · SIH-RJ |
| 👶 **SINASC** | DATASUS | Nascidos vivos | Anual | SINASC |
| ⚰️ **SIM** | DATASUS | Mortalidade | Anual | SIM-DO · SIM-DOFET |
| 🗺️ **IBGE** | IBGE | Censo + Malhas | Decenal | Censo 2022/2010 · Geobr |

---

## ✨ Diferenciais

<table>
<tr>
<td width="50%">

### 🚀 Praticidade

- **Duplo clique** para abrir (`.bat` no Windows, `.sh` no Linux/Mac)
- **Instalação automática** de pacotes na 1ª execução
- **Navegador abre sozinho** ao iniciar
- **Manual embutido** em aba dedicada
- **Roda 100% offline** depois do primeiro uso

### 📦 Formato Parquet

- **~80% menor** que CSV equivalente
- **Leitura colunar** — consultas mais rápidas
- **Comprimido** automaticamente (Snappy)
- **Nativo** de Arrow, DuckDB, Pandas, Polars, Power BI, Tableau, Spark
- **Autocontido** — um arquivo = uma extração

</td>
<td width="50%">

### 🔗 Integração

- **Mesmo código IBGE** (6 dígitos) em todos os apps
- **Mesma pasta** `Tabelas_Todas/` compartilhada
- **Mesmo padrão de nome** de arquivo
- **Mesmo parquet de municípios** (`_METADADOS/`)
- **Joins triviais** entre SIA, SIH, SINASC, SIM e IBGE

### 🛡️ Robustez

- **`tryCatch`** em todas as chamadas de rede
- **Timeout configurável** (600s padrão)
- **Cache liberado** a cada mês/ano (evita estouro de memória)
- **Log detalhado** no console e na interface
- **Avisos amigáveis** em vez de erros crípticos

</td>
</tr>
</table>



Exemplo: SINASC

<img width="1282" height="606" alt="sinasc_1" src="https://github.com/user-attachments/assets/c068acbb-df9c-4105-8791-e4eb82200484" />

<img width="1282" height="606" alt="sinasc_2" src="https://github.com/user-attachments/assets/450615d8-9b71-4241-b62c-0b761fdc8db3" />

<img width="1282" height="622" alt="sinasc_3" src="https://github.com/user-attachments/assets/55ff592f-53c9-4769-ba6c-54c7193e2103" />

<img width="1362" height="702" alt="sinasc_5" src="https://github.com/user-attachments/assets/a4ed26c6-5ae6-443d-87c7-db94f6468c3c" />

<img width="1362" height="712" alt="sinasc_6" src="https://github.com/user-attachments/assets/d0d576a5-ba98-47c0-904f-dc9bd052f316" />

<img width="970" height="520" alt="sinasc_7" src="https://github.com/user-attachments/assets/7fc10bad-4dfa-424f-aa01-c7f6632050d0" />

<img width="1362" height="672" alt="sinasc_8" src="https://github.com/user-attachments/assets/c125bc6b-ddd2-4d5a-903c-ed99f329f49b" />

<img width="1286" height="634" alt="sinasc_10" src="https://github.com/user-attachments/assets/0eb619cc-d015-4942-9aa2-70759f481d7c" />

<img width="1282" height="600" alt="sinasc_12" src="https://github.com/user-attachments/assets/e84c9244-5de0-45ba-840a-05f655f29c6b" />



## 🚀 DASHBOARD de ANALISE

<img width="1308" height="594" alt="sinasc_dash" src="https://github.com/user-attachments/assets/aa8442ce-664c-43cc-b20c-4330f2cb8418" />

<img width="1300" height="602" alt="sinasc_dash (2)" src="https://github.com/user-attachments/assets/4ecfde8f-ebc1-40ad-b2ac-5ecd54878b2f" />

<img width="1312" height="678" alt="sinasc_dash 3" src="https://github.com/user-attachments/assets/02f2596b-e99f-4011-8ebf-d9d84331d0fa" />

<img width="1362" height="684" alt="sinasc_dash 4" src="https://github.com/user-attachments/assets/2b274316-e967-49ac-b39f-2d5e2a23f86f" />

<img width="1362" height="682" alt="sinasc_dash 5" src="https://github.com/user-attachments/assets/e9c4867e-6acf-4ca7-8d60-b810c6f5ff8d" />

<img width="1362" height="698" alt="sinasc_dash 6" src="https://github.com/user-attachments/assets/000f5c12-c0d6-47c2-a51b-10e43cd275e6" />

<img width="1362" height="696" alt="sinasc_dash 7" src="https://github.com/user-attachments/assets/7cf80f28-585d-4d32-a1d2-dd723327a71b" />

<img width="1362" height="696" alt="sinasc_dash 8" src="https://github.com/user-attachments/assets/17ab7310-97d8-4f87-80a5-e7e509a81028" />















---

## 🚀 Como começar

### 📋 Pré-requisitos

- **R 4.0+** — [Windows](https://cran.r-project.org/bin/windows/base/) · [macOS](https://cran.r-project.org/bin/macosx/) · [Linux](https://cran.r-project.org/bin/linux/)
- Conexão com a internet (só para o primeiro uso e downloads)

### 🖱️ Instalação rápida (usuário final)

1. **Baixe** este repositório: botão verde **Code** → **Download ZIP**
2. **Extraia** o ZIP em uma pasta de sua preferência
3. **Duplo clique** em `Iniciar_App.bat` (Windows) ou `iniciar_app.sh` (Linux/Mac)
4. O navegador abre automaticamente 🎉

> ⏳ **Primeira execução:** o app instala os pacotes do R automaticamente. Pode levar de 5 a 20 minutos, dependendo da conexão. Depois disso, abre em segundos.

### 💻 Instalação avançada (desenvolvedor)

```r
# Clone o repositório
git clone https://github.com/seu-usuario/extratores-datasus.git
cd extratores-datasus

# Abra no RStudio
# File → Open Project → selecione ExtratorSIA/ExtratorSIA.Rproj

# Ou rode direto no R
setwd("ExtratorSIA")
shiny::runApp(launch.browser = TRUE)
