#!/bin/sh
#Currently this script needs to be run at project root!

Rscript -e "library(tmcRtestrunner);run_tests(\"$PWD\", print=TRUE)"
