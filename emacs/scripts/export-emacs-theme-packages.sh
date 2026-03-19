#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

/Applications/Emacs.app/Contents/MacOS/Emacs --batch -Q -l "$SCRIPT_DIR/export-emacs-theme-packages.el"
