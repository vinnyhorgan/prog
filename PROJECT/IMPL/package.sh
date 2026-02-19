#!/usr/bin/env bash
set -euo pipefail

PKG_DIR="Horgan_Daniel_Vincent_928744_SSSP_LP_202602"
ZIP_FILE="${PKG_DIR}.zip"

if ! command -v zip >/dev/null 2>&1; then
  echo "Errore: comando 'zip' non disponibile." >&2
  exit 1
fi

rm -rf "${PKG_DIR}"
rm -f "${ZIP_FILE}"

mkdir -p "${PKG_DIR}/Prolog" "${PKG_DIR}/Lisp"

cp Gruppo.txt "${PKG_DIR}/Gruppo.txt"
cp Prolog/sssp.pl "${PKG_DIR}/Prolog/sssp.pl"
cp Prolog/README.txt "${PKG_DIR}/Prolog/README.txt"
cp Lisp/sssp.lisp "${PKG_DIR}/Lisp/sssp.lisp"
cp Lisp/README.txt "${PKG_DIR}/Lisp/README.txt"

zip -r "${ZIP_FILE}" "${PKG_DIR}" >/dev/null
