#!/bin/bash
cd "`dirname $0`"
runhaskell -hide-all-packages -packagebase -packagetransformers -packageHUnit -packageQuickCheck -packagehspec -i../src Spec.hs $*
