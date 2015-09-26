#!/bin/bash
#
# (C) 2011-14 Nicola Bonelli <nicola@pfq.io>

# set -x
set -e

API_DIR="api"
LANG_DIR="lang"
DOC_VERSION="v5.2"
BOLD=`tput bold`
RESET=`tput sgr0`

function echoB {
    echo "${BOLD}${1}${RESET}"
}

echoB "Creating folders:"

rm -rf ${API_DIR}
rm -rf ${LANG_DIR}

mkdir -p api/c
mkdir -p api/cpp
mkdir -p api/haskell
mkdir -p lang/cpp
mkdir -p lang/haskell

echoB "C: library docs:"

cd ../user && cat ../docs/c-api.doxy | doxygen -

echoB "C++: library docs:"

cd ../user && cat ../docs/cpp-api.doxy | doxygen -

echoB "C++: Q-Lang docs:"

cd ../user && cat ../docs/cpp-lang.doxy | doxygen -

echoB "Haskell: library docs:" 
cd ../user/Haskell && runhaskell Setup configure --user && runhaskell Setup build && haddock -h -o ../../docs/api/haskell  -t "PFQ Library ${DOC_VERSION}" -p ../../docs/library.haddock dist/build/Network/PFq.hs
cd -

echoB "Haskell: Q-Lang docs:" 
cd ../user/Haskell && haddock -h -o ../../docs/lang/haskell -t "pfq-lang ${DOC_VERSION}" -p ../../docs/lang.haddock Network/PFq/Default.hs Network/PFq/Experimental.hs

echoB "pfq API documentation created in ${API_DIR}."
echoB "pfq-lang documentation created in ${LANG_DIR}."


