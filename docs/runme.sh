#!/bin/bash
#
# (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>

set -x
set -e

echo "Creating folders..."

rm -rf api

mkdir -p api/c
mkdir -p api/cpp
mkdir -p api/haskell

echo "Generating C library docs"

cat cpp-api.doxy | doxygen -

echo "Generating C++ library docs"

cat c-api.doxy | doxygen -

echo "Generating Haskell library docs"

cd ../user/Haskell && haddock -h -o ../../docs/api/haskell Network/PFq.hs 
