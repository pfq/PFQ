#!/bin/sh

echo "[pfq] running hsc2hs tool..."

/usr/bin/hsc2hs Network/PFq.hsc 

echo "[pfq] compiling examples..."

/usr/bin/ghc -O3 pfq-read.hs     -o pfq-read     -lpfq -threaded -with-rtsopts="-N" 
/usr/bin/ghc -O3 pfq-counters.hs -o pfq-counters -lpfq -threaded -with-rtsopts="-N" 
/usr/bin/ghc -O3 pfq-dispatch.hs -o pfq-dispatch -lpfq -threaded -with-rtsopts="-N" 
