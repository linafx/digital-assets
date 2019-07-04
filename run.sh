#! /bin/bash

bazel run //ledger/ledger-api-test-tool -- -p 10011 -sp --timeout-scale-factor $*

