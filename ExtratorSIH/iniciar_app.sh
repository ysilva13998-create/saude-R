#!/usr/bin/env bash
# Extrator SIH - Iniciador para Linux/Mac
# chmod +x iniciar_app.sh

echo "======================================================================"
echo "   EXTRATOR SIH - DATASUS"
echo "======================================================================"

cd "$(dirname "$(readlink -f "$0" 2>/dev/null || echo "$0")")"

if ! command -v Rscript >/dev/null 2>&1; then
  echo ""
  echo "[ERRO] R nao encontrado."
  echo "Instale o R: https://cran.r-project.org/"
  echo ""
  read -r -p "Pressione Enter para sair..."
  exit 1
fi

echo "[OK] R encontrado: $(command -v Rscript)"
echo ""
echo "Iniciando o aplicativo..."
echo "Para encerrar: pressione Ctrl+C aqui."
echo "======================================================================"

Rscript -e "shiny::runApp(getwd(), launch.browser=TRUE)"

echo ""
echo "Aplicativo encerrado."
read -r -p "Pressione Enter para sair..."