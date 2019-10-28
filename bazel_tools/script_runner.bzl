# Copyright (c) 2019 The DAML Authors. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

load("@bazel_skylib//lib:paths.bzl", "paths")

bash_runfiles_init = r"""
# --- begin runfiles.bash initialization v2 ---
# Copy-pasted from the Bazel Bash runfiles library v2.
set -uo pipefail; f=bazel_tools/tools/bash/runfiles/runfiles.bash
source "${RUNFILES_DIR:-/dev/null}/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "${RUNFILES_MANIFEST_FILE:-/dev/null}" | cut -f2- -d' ')" 2>/dev/null || \
  source "$0.runfiles/$f" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  source "$(grep -sm1 "^$f " "$0.exe.runfiles_manifest" | cut -f2- -d' ')" 2>/dev/null || \
  { echo>&2 "ERROR: cannot find $f"; exit 1; }; f=; set -e
# --- end runfiles.bash initialization v2 ---
"""

def script_runner(ctx, name, script, data = [], collect_data = True, interpreter = None, cc_runfiles = None, is_test = False):
    """Create an executable that runs a script.

    Rules using this function must have an attribute `_cc_toolchain` containing
    "@bazel_tools//tools/cpp:current_cc_toolchain", and they must depend on the
    `cpp` `fragments`.

    Args:
      ctx: Rule context.
      name: string, the name of the produced binary.
      script: File, the script file.
      interpreter: string or path or File or Target, the script interpreter.
        Defaults to shell defined by "@bazel_tools//tools/sh:toolchain_type".
      cc_runfiles: Target, the "@bazel_tools//tools/cpp/runfiles" target.
        Defaults to `ctx.attr._cc_runfiles`.
      is_test: Bool, whether the output is a test binary.
        See https://github.com/bazelbuild/bazel/blob/1.1.0/tools/cpp/runfiles/runfiles_src.h#L39-L42

    Returns:
      (executable, runfiles)
        executable: File, the created binary.
        runfiles: runfiles, collecting runtime dependencies determined by
          `script`, `data`, `collect_data`, and `interpreter`.
    """
    script_path = "%s/%s" % (ctx.workspace_name, script.short_path)

    runfiles = ctx.runfiles(files = [script] + data, collect_data = collect_data)

    if interpreter == None:
        sh_toolchain = ctx.toolchains["@bazel_tools//tools/sh:toolchain_type"]
        interpreter = sh_toolchain.path

    interpreter_type = type(interpreter)
    if interpreter_type == "string" or interpreter_type == "path":
        if paths.is_absolute(interpreter):
            interpreter_path = interpreter
        else:
            interpreter_path = "%s/%s" % (ctx.workspace_name, interpreter)
    elif interpreter_type == "File":
        interpreter_path = "%s/%s" %(ctx.workspace_name, interpreter.short_path)
    elif interpreter_type == "Target":
        interpreter_info = interpreter[DefaultInfo]
        interpreter_exec = interpreter_info.files_to_run.executable
        interpreter_path = "%s/%s" %(ctx.workspace_name, interpreter_exec.short_path)
        runfiles = runfiles.merge(interpreter_info.data_runfiles)
    else:
        fail("interpreter must be string or path or File or Target, but got %s." % interpreter_type)

    if cc_runfiles == None:
        cc_runfiles = ctx.attr._cc_runfiles

    c_source = ctx.actions.declare_file("run_%s.cc" % script.basename, sibling = script)
    ctx.actions.write(c_source, content = r"""
#include <iostream>
#include <memory>
#include <string>

#include <unistd.h>

#include "tools/cpp/runfiles/runfiles.h"
using bazel::tools::cpp::runfiles::Runfiles;

int main(int argc, char** argv) {{
    std::string error;
    std::unique_ptr<Runfiles> runfiles(
        {is_test} ? Runfiles::CreateForTest(&error) : Runfiles::Create(argv[0], &error)
    );
    if (runfiles == nullptr) {{
        std::cerr << "Bazel runfiles initialization failed:\n  " << error << "\n";
        return 1;
    }}

    std::string interpreter = runfiles->Rlocation("{interpreter_path}");
    std::string script = runfiles->Rlocation("{script_path}");

    std::unique_ptr<char*[]> argv_new(new char*[argc + 2]);
    argv_new[0] = const_cast<char*>(interpreter.c_str());
    argv_new[1] = const_cast<char*>(script.c_str());
    for (int i = 1; i < argc; ++i) {{
        argv_new[i + 1] = argv[i];
    }}
    argv_new[argc + 1] = nullptr;

#ifdef _WIN32
    // On Windows execv does not behave as expected,
    // see https://github.com/austriancoder/ccache-win32/issues/3#issuecomment-120084234.
    int status = spawnv(_P_WAIT, argv_new[0], argv_new.get());
    if (errno) {{
        perror("Failed to invoke interpreter");
        return 1;
    }}
    return status;
#else
    execv(argv_new[0], argv_new.get());
    perror("Failed to invoke interpreter");
    return 1;
#endif
}}
""".format(
        is_test = "true" if is_test else "false",
        interpreter_path = interpreter_path,
        script_path = script_path,
    ))

    cc_toolchain = ctx.attr._cc_toolchain[cc_common.CcToolchainInfo]
    feature_configuration = cc_common.configure_features(ctx = ctx, cc_toolchain = cc_toolchain)
    (compilation_context, compilation_outputs) = cc_common.compile(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        srcs = [c_source],
        compilation_contexts = [cc_runfiles[CcInfo].compilation_context],
        name = name,
    )
    linking_outputs = cc_common.link(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = compilation_outputs,
        linking_contexts = [cc_runfiles[CcInfo].linking_context],
        name = name,
        output_type = 'executable',
        link_deps_statically = True,
    )

    return linking_outputs.executable, runfiles
