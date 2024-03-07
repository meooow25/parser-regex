#!/bin/bash
set -eu

echo "Downloading CaseFolding.txt..."
curl --create-dirs \
  --output script/script-data/CaseFolding.txt \
  https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt
echo "Downloaded"
echo "Generating hs..."
cabal run script/GenCaseFold.hs -- \
  script/script-data/CaseFolding.txt \
  src/Regex/Internal/Generated/CaseFold.hs \
  Regex.Internal.Generated.CaseFold
