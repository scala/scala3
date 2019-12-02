#!/bin/sh
cd "$(dirname "$0")"
if [ ! -d "node_modules" ]; then
  npm install
fi
npm run build
echo "Copying CSS result..."
cp target/bootstrap.min.css ../resources/css/bootstrap.min.css

# Update the generated website without running sbt
gen_dir=../../docs/_site/css
if [ -d "$gen_dir" ]; then
  cp ../resources/css/* $gen_dir/
fi

echo "Bootstrap CSS updated."
