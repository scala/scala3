#!/bin/sh
cd "$(dirname "$0")"
npm run build
echo "Copying css file..."
cp target/bootstrap.min.css ../resources/css/bootstrap.min.css
echo "Bootstrap css updated."
