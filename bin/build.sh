#!/usr/bin/env bash

# Stop if any command fails.
set -e

# Stop on unset variables.
set -u

# Be in project root.
cd "${0%/*}/.."

# Have dependencies from npm ready.
npm i

# Define what version we are building.
VERSION=$(date +%s)

# Have clean distribution directory.
rm -r dist || true

# Copy static resources.
cp -r src/_dist dist
mv dist/static/VERSION "dist/static/$VERSION"

# Pass version to application.
sed -i "" "s/VERSION/$VERSION/g" dist/index.html

# Compile application.
elm make src/Main.elm --output "dist/static/$VERSION/elm.js" --optimize
elm-ffi "dist/static/$VERSION/elm.js"
{
  uglifyjs "dist/static/$VERSION/elm.js" --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,passes=2,unsafe_comps,unsafe'
  uglifyjs "dist/static/$VERSION/main.js" --compress
} | uglifyjs --mangle --output "dist/static/$VERSION/main.js"
rm "dist/static/$VERSION/elm.js"
