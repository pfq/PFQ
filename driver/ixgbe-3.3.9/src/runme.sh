#!/bin/bash

set -e

PROGNAME=$(basename $0)

MODSYM=../../../kernel/Module.symvers

function error_exit
{
    echo "${PROGNAME}: ${1:-"Unknown Error"}" 1>&2
    exit 1
}

if [ ! -r ${MODSYM} ]; then
    error_exit "Compile PFQ kernel module first!"    
fi 

/bin/cp ${MODSYM} .

echo "[*] Compiling the PFQ aware driver..."

if [ -x /usr/bin/colormake ]; then 
    /usr/bin/colormake V=1 -j12
else   
    /usr/bin/make V=1 -j12
fi
