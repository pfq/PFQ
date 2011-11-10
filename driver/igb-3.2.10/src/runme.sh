#!/bin/sh
set -e
cp ../../../kernel/Module.symvers .
make -j12

