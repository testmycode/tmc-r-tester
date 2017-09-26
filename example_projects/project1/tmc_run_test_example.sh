#!/bin/sh
#Currently this script needs to be run at project root!

Rscript -e "library(tmcRtestrunner);runTests(\"$PWD\", print=TRUE)"
