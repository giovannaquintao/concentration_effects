#!/usr/bin/env bash
# Compila misc/main.tex e copia misc/main.pdf para a pasta da tese no iCloud.
# Uso: bash compile_export.sh [--force]   (--force copia mesmo com erros de LaTeX)

set -u

PROJ="$(cd "$(dirname "$0")/../../.." && pwd)"
DEST="/c/Users/giova/iCloudDrive/PhD/Thesis"
FORCE="${1:-}"

cd "$PROJ/misc" || { echo "ERRO: pasta misc/ nao encontrada em $PROJ"; exit 1; }

timeout 300 latexmk -pdf -interaction=nonstopmode main.tex > /dev/null 2>&1

if grep -q "I can't write on file .main.pdf" main.log; then
  echo "ERRO: main.pdf esta aberto e travado por um visualizador de PDF. Feche e rode de novo."
  exit 2
fi
if ! grep -q "Output written on main.pdf" main.log; then
  echo "ERRO: o LaTeX nao gerou o main.pdf. Erros:"
  grep -A3 -E '^!' main.log | head -20
  exit 3
fi

paginas=$(grep -o 'Output written on main.pdf ([0-9]* pages' main.log | grep -o '[0-9]*')
erros=$(grep -E '^!' main.log | sort -u)
indefinidas=$(grep -E 'Reference .* undefined|Citation .* undefined' main.log | sort -u)

echo "Compilado: $paginas paginas."
if [ -n "$erros" ]; then
  echo "Erros de LaTeX:"; echo "$erros"
  grep -A4 -E '^!' main.log | grep -E '^l\.[0-9]+' | sort -u | head -5
fi
if [ -n "$indefinidas" ]; then
  echo "Referencias ou citacoes indefinidas:"; echo "$indefinidas"
fi

if [ -n "$erros" ] && [ "$FORCE" != "--force" ]; then
  echo "NAO EXPORTADO: ha erros de LaTeX. Rode com --force para copiar mesmo assim."
  exit 4
fi

mkdir -p "$DEST"
cp -f main.pdf "$DEST/main.pdf" && echo "Exportado: $DEST/main.pdf"
