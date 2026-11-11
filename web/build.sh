#!/bin/sh
# Build the browser demo into a static directory (default: _site).
set -eu
cd "$(dirname "$0")/.."
out=${1:-_site}
dune build --profile release web/src/vansh_js.bc.js
rm -rf "$out"
mkdir -p "$out"
cp web/index.html web/style.css web/app.js "$out"/
[ -f web/og.png ] && cp web/og.png "$out"/
cp _build/default/web/src/vansh_js.bc.js "$out/vansh.js"
chmod u+w "$out/vansh.js"
python3 web/gen_samples.py > "$out/samples.js"
echo "built $out ($(wc -c < "$out/vansh.js" | tr -d ' ') byte analyzer)"
