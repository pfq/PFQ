#!/bin/bash
#
# (C) 2011-14 Nicola Bonelli <nicola.bonelli@cnit.it>

set -x
set -e

echo "Creating folders..."

rm -rf api

mkdir -p api/C
mkdir -p api/C++
mkdir -p api/Haskell

echo "Generating C library docs"

cat cpp-api.doxy | doxygen -

echo "Generating C++ library docs"

cat c-api.doxy | doxygen -

echo "Generating Haskell library docs"

cd ../user/Haskell && haddock -h -o ../../docs/api/Haskell Network/PFq.hs 
