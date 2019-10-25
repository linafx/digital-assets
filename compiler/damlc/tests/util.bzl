# Copyright (c) 2019 The DAML Authors. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

load("//bazel_tools:haskell.bzl", "da_haskell_test")
load("//bazel_tools:script_runner.bzl", "bash_runfiles_init", "script_runner")

def _damlc_compile_test_impl(ctx):
    stack_opt = "-K" + ctx.attr.stack_limit if ctx.attr.stack_limit else ""
    heap_opt = "-M" + ctx.attr.heap_limit if ctx.attr.heap_limit else ""
    script = """
      {runfiles_init}
      set -eou pipefail

      DAMLC=$(rlocation $TEST_WORKSPACE/{damlc})
      MAIN=$(rlocation $TEST_WORKSPACE/{main})

      TMP=$(mktemp -d)
      function cleanup() {{
        rm -rf "$TMP"
      }}
      trap cleanup EXIT

      $DAMLC compile $MAIN -o $TMP/out +RTS -s {stack_opt} {heap_opt}
    """.format(
        damlc = ctx.executable.damlc.short_path,
        main = ctx.files.main[0].short_path,
        stack_opt = stack_opt,
        heap_opt = heap_opt,
        runfiles_init = bash_runfiles_init,
    )

    script_file = ctx.actions.declare_file("%s.sh" % ctx.label.name)
    ctx.actions.write(output = script_file, content = script)
    executable, runfiles = script_runner(
        ctx,
        ctx.label.name,
        script_file,
        is_test = True,
        data = ctx.files._sh_runfiles + ctx.files.main + ctx.files.srcs,
    )
    damlc_runfiles = ctx.attr.damlc[DefaultInfo].data_runfiles
    runfiles = runfiles.merge(damlc_runfiles)
    return [DefaultInfo(executable = executable, runfiles = runfiles)]

damlc_compile_test = rule(
    implementation = _damlc_compile_test_impl,
    attrs = {
        "srcs": attr.label_list(allow_files = True),
        "main": attr.label(allow_files = True),
        "damlc": attr.label(
            default = Label("//compiler/damlc"),
            executable = True,
            cfg = "target",
            allow_files = True,
        ),
        "stack_limit": attr.string(),
        "heap_limit": attr.string(),
        "_cc_runfiles": attr.label(
            default = Label("@bazel_tools//tools/cpp/runfiles"),
        ),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
        "_sh_runfiles": attr.label(
            default = Label("@bazel_tools//tools/bash/runfiles"),
        ),
    },
    fragments = ["cpp"],
    test = True,
    toolchains = ["@bazel_tools//tools/sh:toolchain_type"],
)

def damlc_integration_test(name, main_function):
    da_haskell_test(
        name = name,
        size = "large",
        srcs = ["src/DA/Test/DamlcIntegration.hs"],
        src_strip_prefix = "src",
        main_function = main_function,
        data = [
            "//compiler/damlc/pkg-db",
            "//compiler/scenario-service/server:scenario_service_jar",
            "@jq_dev_env//:jq",
            ":daml-test-files",
            ":bond-trading",
            ":query-lf-lib",
        ],
        deps = [
            "//compiler/daml-lf-ast",
            "//compiler/daml-lf-proto",
            "//compiler/damlc/daml-compiler",
            "//compiler/damlc/daml-ide-core",
            "//compiler/damlc/daml-lf-conversion",
            "//compiler/damlc/daml-opts:daml-opts-types",
            "//compiler/damlc/daml-opts",
            "//daml-lf/archive:daml_lf_dev_archive_haskell_proto",
            "//libs-haskell/bazel-runfiles",
            "//libs-haskell/da-hs-base",
            "//libs-haskell/test-utils",
        ],
        hackage_deps = [
            "aeson-pretty",
            "base",
            "bytestring",
            "containers",
            "deepseq",
            "directory",
            "dlist",
            "extra",
            "filepath",
            "ghc-lib",
            "ghc-lib-parser",
            "ghcide",
            "haskell-lsp-types",
            "optparse-applicative",
            "process",
            "proto3-suite",
            "shake",
            "tagged",
            "tasty",
            "tasty-hunit",
            "text",
            "time",
        ],
        visibility = ["//visibility:public"],
    )
