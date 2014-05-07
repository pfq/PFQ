#!/bin/bash

set -x

rm -rf api
mkdir -p api/C
mkdir -p api/C++
mkdir -p api/Haskell
cat cpp-api.doxy | doxygen -
cat c-api.doxy | doxygen -
