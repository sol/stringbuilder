#!/bin/bash
cd "`dirname $0`"
runhaskell -hide-all-packages -packagebase -packagetransformers -packagehspec-shouldbe -i../src Spec.hs $*
