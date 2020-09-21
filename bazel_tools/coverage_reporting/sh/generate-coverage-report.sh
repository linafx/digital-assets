#!/bin/bash

echo "IMPORTANT: This must be running from daml root directory"
echo "This is an example how to generate HTML coverage report for JACOCO collected runtime coverage data"
echo

if [[ -f report/jacoco.exec ]]
then
    echo "JACOCO exec file found, going forward with report generation"

    # build the singlejar for the server process's target
    bazel build //ledger/ledger-on-memory:app_deploy.jar

    # this will grab all the relevant class files and copy to the report/class directory. These needed for report generation.
    unzip bazel-bin/ledger/ledger-on-memory/app_deploy.jar com/daml/* -d report/class

    # collectsrc collects all the relevant daml source filed and copies into report/src to the right place. This is needed for report generation.
    bazel run //bazel_tools/coverage_reporting:run_collectsrc -- $(pwd) $(pwd)/report/src

    # running the JACOCO CLI and generate the report into report/html
    java -jar $(pwd)/bazel_tools/coverage_reporting/lib/jacococli.jar \
      report \
      report/jacoco.exec \
      --classfiles report/class \
      --html report/html \
      --sourcefiles report/src
else
    echo "JACOCO exec file not found, first generate runtime coverage data."
    echo
    echo "How to run the server process for conformance testing:"
    cat << EOF
bazel run \\
  //ledger/ledger-on-memory:app \\
  -- \\
  --jvm_flag="-javaagent:$(pwd)/bazel_tools/coverage_reporting/lib/jacocoagent.jar=destfile=$(pwd)/report/jacoco.exec" \\
  --contract-id-seeding=testing-weak \\
  --participant participant-id=example,port=6865 \\
  --batching enable=true,max-batch-size-bytes=262144
EOF
    echo
    echo "IMPORTANT: this process need to stop and being stopped gracefuly because JACOCO agent is generating the destfile with a shutdown hook! Crtl-C is doing a trick in a terminal."
    echo
    echo "How to run the client process for conformance testing:"
    echo bazel run //ledger/ledger-api-test-tool -- --verbose localhost:6865
    echo
    echo "Coverage report NOT generated."
fi




