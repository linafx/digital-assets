workspace(
    name = "com_github_digital_asset_daml",
    managed_directories = {
        "@npm": ["node_modules"],
        "@daml_extension_deps": ["compiler/daml-extension/node_modules"],
        "@navigator_frontend_deps": ["navigator/frontend/node_modules"],
    },
)

# NOTE(JM): Load external dependencies from deps.bzl.
# Do not put "http_archive" and similar rules into this file. Put them into
# deps.bzl. This allows using this repository as an external workspace.
# (though with the caviat that that user needs to repeat the relevant bits of
#  magic in this file, but at least right versions of external rules are picked).
load("//:deps.bzl", "daml_deps")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

daml_deps()

load("@com_google_protobuf//:protobuf_deps.bzl", "protobuf_deps")

protobuf_deps()

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load("@rules_haskell//tools:repositories.bzl", "rules_haskell_worker_dependencies")

# We don't use the worker mode, but this is required for bazel query to function.
rules_haskell_worker_dependencies()

load("//bazel_tools:os_info.bzl", "os_info")

os_info(name = "os_info")

load("@os_info//:os_info.bzl", "is_linux", "is_windows")

http_archive(
    name = "alex",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "alex",
    srcs = glob(["**"]),
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "d58e4d708b14ff332a8a8edad4fa8989cb6a9f518a7c6834e96281ac5f8ff232",
    strip_prefix = "alex-3.2.4",
    urls = ["http://hackage.haskell.org/package/alex-3.2.4/alex-3.2.4.tar.gz"],
)

http_archive(
    name = "happy",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "happy",
    srcs = glob(["**"]),
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "9094d19ed0db980a34f1ffd58e64c7df9b4ecb3beed22fd9b9739044a8d45f77",
    strip_prefix = "happy-1.19.11",
    urls = ["http://hackage.haskell.org/package/happy-1.19.11/happy-1.19.11.tar.gz"],
)

http_archive(
    name = "hpp",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "hpp",
    srcs = glob(["**"]),
    deps = [
        "@stackage//:base",
        "@stackage//:directory",
        "@stackage//:filepath",
        "@stackage//:hpp",
        "@stackage//:time",
    ],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "d1a843f4383223f85de4d91759545966f33a139d0019ab30a2f766bf9a7d62bf",
    strip_prefix = "hpp-0.6.1",
    urls = ["http://hackage.haskell.org/package/hpp-0.6.1/hpp-0.6.1.tar.gz"],
)

http_archive(
    name = "c2hs",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary")
haskell_cabal_binary(
    name = "c2hs",
    srcs = glob(["**"]),
    deps = [
        "@c2hs_deps//:base",
        "@c2hs_deps//:bytestring",
        "@c2hs_deps//:language-c",
        "@c2hs_deps//:filepath",
        "@c2hs_deps//:dlist",
    ],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "91dd121ac565009f2fc215c50f3365ed66705071a698a545e869041b5d7ff4da",
    strip_prefix = "c2hs-0.28.6",
    urls = ["http://hackage.haskell.org/package/c2hs-0.28.6/c2hs-0.28.6.tar.gz"],
)

http_archive(
    name = "proto3_suite",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary", "haskell_cabal_library")
haskell_cabal_binary(
    name = "compile-proto-file",
    srcs = glob(["**"]),
    deps = [
        "@stackage//:base",
        "@stackage//:optparse-applicative",
        "@stackage//:proto3-suite",
        "@stackage//:system-filepath",
        "@stackage//:text",
        "@stackage//:turtle",
    ],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "9437bd383e58a777a6cd857202007985317299bfc16ccf81bc0015b964092f40",
    strip_prefix = "proto3-suite-3f6dd6f612cf2eba3c05798926ff924b0d5ab4fa",
    urls = ["https://github.com/awakesecurity/proto3-suite/archive/3f6dd6f612cf2eba3c05798926ff924b0d5ab4fa.tar.gz"],
)

http_archive(
    name = "grpc_haskell_core",
    build_file_content = """
load("@com_github_digital_asset_daml//bazel_tools:fat_cc_library.bzl", "fat_cc_library")
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "grpc-haskell-core",
    version = "0.0.0.0",
    srcs = glob(["**"]),
    deps = packages["grpc-haskell-core"].deps + [
        ":grpc",
        ":libgpr",
    ],
    tools = ["@c2hs//:c2hs"],
    visibility = ["//visibility:public"],
)
fat_cc_library(
    name = "grpc",
    input_lib = "@com_github_grpc_grpc//:grpc",
)
# Cabal requires libgpr next to libgrpc. However, fat_cc_library of grpc
# already contains gpr and providing a second copy would cause duplicate symbol
# errors. Instead, we define an empty dummy libgpr.
genrule(name = "gpr-source", outs = ["gpr.c"], cmd = "touch $(OUTS)")
cc_library(name = "gpr", srcs = [":gpr-source"])
cc_library(name = "libgpr", srcs = [":gpr"])
    """,
    patch_args = ["-p2"],
    patches = [
        "@com_github_digital_asset_daml//bazel_tools:grpc-haskell-core-mask-runops.patch",
        "@com_github_digital_asset_daml//bazel_tools:grpc-haskell-core-cpp-options.patch",
    ],
    sha256 = "c6201f4e2fd39f25ca1d47b1dac4efdf151de88a2eb58254d61abc2760e58fda",
    strip_prefix = "gRPC-haskell-11681ec6b99add18a8d1315f202634aea343d146/core",
    urls = ["https://github.com/awakesecurity/gRPC-haskell/archive/11681ec6b99add18a8d1315f202634aea343d146.tar.gz"],
)

http_archive(
    name = "ghcide",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_binary", "haskell_cabal_library")
deps = [
    "@stackage//:aeson",
    "@stackage//:async",
    "@stackage//:base",
    "@stackage//:binary",
    "@stackage//:bytestring",
    "@stackage//:containers",
    "@stackage//:data-default",
    "@stackage//:deepseq",
    "@stackage//:directory",
    "@stackage//:extra",
    "@stackage//:filepath",
    "@stackage//:ghc",
    "@stackage//:ghc-boot",
    "@stackage//:ghc-boot-th",
    "@stackage//:ghc-paths",
    "@stackage//:hashable",
    "@stackage//:haskell-lsp",
    "@stackage//:haskell-lsp-types",
    "@stackage//:hie-bios",
    "@stackage//:mtl",
    "@stackage//:network-uri",
    "@stackage//:optparse-applicative",
    "@stackage//:prettyprinter",
    "@stackage//:prettyprinter-ansi-terminal",
    "@stackage//:rope-utf16-splay",
    "@stackage//:safe-exceptions",
    "@stackage//:shake",
    "@stackage//:sorted-list",
    "@stackage//:stm",
    "@stackage//:syb",
    "@stackage//:text",
    "@stackage//:time",
    "@stackage//:transformers",
    "@stackage//:unordered-containers",
    "@stackage//:utf8-string",
]
haskell_cabal_library(
    name = "ghcide-lib",
    package_name = "ghcide",
    version = "0.0.3",
    srcs = glob(["**"]),
    deps = deps,
    visibility = ["//visibility:public"],
)
haskell_cabal_binary(
    name = "ghcide",
    srcs = glob(["**"]),
    deps = deps + [":ghcide-lib"],
    visibility = ["//visibility:public"],
)
    """,
    sha256 = "82fc58fb03f10dd7795bf1aabd5d9f3175f60240968b9fa8755fd08a49ce8f07",
    strip_prefix = "ghcide-dcd7cb499e33273e1d5835ea1e69907e8224e483",
    urls = ["https://github.com/digital-asset/ghcide/archive/dcd7cb499e33273e1d5835ea1e69907e8224e483.tar.gz"],
)

http_archive(
    name = "ghcide_ghc_lib",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@rules_haskell//haskell:defs.bzl", "haskell_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "ghcide",
    version = "0.0.3",
    srcs = glob(["**"]),
    flags = packages["ghcide"].flags,
    deps = packages["ghcide"].deps,
    visibility = ["//visibility:public"],
)
haskell_library(
    name = "testing",
    srcs = glob(["test/src/**/*.hs"]),
    src_strip_prefix = "test/src",
    deps = [
        "@stackage//:base",
        "@stackage//:extra",
        "@stackage//:containers",
        "@stackage//:haskell-lsp-types",
        "@stackage//:lens",
        "@stackage//:lsp-test",
        "@stackage//:parser-combinators",
        "@stackage//:tasty-hunit",
        "@stackage//:text",
    ],
    compiler_flags = [
       "-XBangPatterns",
       "-XDeriveFunctor",
       "-XDeriveGeneric",
       "-XGeneralizedNewtypeDeriving",
       "-XLambdaCase",
       "-XNamedFieldPuns",
       "-XOverloadedStrings",
       "-XRecordWildCards",
       "-XScopedTypeVariables",
       "-XStandaloneDeriving",
       "-XTupleSections",
       "-XTypeApplications",
       "-XViewPatterns",
    ],
    visibility = ["//visibility:public"],
)
    """,
    patch_args = ["-p1"],
    patches = ["@com_github_digital_asset_daml//bazel_tools:haskell-ghcide-expose-compat.patch"],
    sha256 = "82fc58fb03f10dd7795bf1aabd5d9f3175f60240968b9fa8755fd08a49ce8f07",
    strip_prefix = "ghcide-dcd7cb499e33273e1d5835ea1e69907e8224e483",
    urls = ["https://github.com/digital-asset/ghcide/archive/dcd7cb499e33273e1d5835ea1e69907e8224e483.tar.gz"],
)

load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    extra_deps = {
        "bzlib-conduit": ["@bzip2//:libbz2"],
        "digest": ["@com_github_madler_zlib//:libz"],
        "zlib": ["@com_github_madler_zlib//:libz"],
    },
    flags = dicts.add(
        {
            "ghcide": ["ghc-lib"],
            "hlint": ["ghc-lib"],
        },
        {
            "blaze-textual": ["integer-simple"],
            "cryptonite": ["-integer-gmp"],
            "hashable": ["-integer-gmp"],
            "integer-logarithms": ["-integer-gmp"],
            "text": ["integer-simple"],
            "scientific": ["integer-simple"],
        } if not is_windows else {},
    ),
    local_snapshot = "//:stack-snapshot.yaml",
    packages = [
        "aeson",
        "aeson-pretty",
        "ansi-terminal",
        "ansi-wl-pprint",
        "array",
        "async",
        "attoparsec",
        "base",
        "base16-bytestring",
        "base64-bytestring",
        "binary",
        "blaze-html",
        "bytestring",
        "Cabal",
        "cereal",
        "clock",
        "cmark-gfm",
        "conduit",
        "conduit-extra",
        "connection",
        "containers",
        "contravariant",
        "cryptohash",
        "cryptonite",
        "data-default",
        "Decimal",
        "deepseq",
        "directory",
        "dlist",
        "either",
        "exceptions",
        "extra",
        "fast-logger",
        "file-embed",
        "filepath",
        "filepattern",
        "foldl",
        "ghc",
        "ghc-boot",
        "ghc-boot-th",
        "ghc-lib",
        "ghc-lib-parser",
        "ghc-paths",
        "ghc-prim",
        "gitrev",
        "grpc-haskell",
        "hashable",
        "haskeline",
        "haskell-lsp",
        "haskell-lsp-types",
        "haskell-src",
        "haskell-src-exts",
        "hie-bios",
        "hlint",
        "hpc",
        "hpp",
        "http-client",
        "http-client-tls",
        "http-conduit",
        "http-types",
        "insert-ordered-containers",
        "lens",
        "lens-aeson",
        "lifted-async",
        "lifted-base",
        "lsp-test",
        "main-tester",
        "managed",
        "megaparsec",
        "memory",
        "monad-control",
        "monad-logger",
        "monad-loops",
        "mtl",
        "neat-interpolation",
        "network",
        "network-uri",
        "nsis",
        "open-browser",
        "optparse-applicative",
        "optparse-generic",
        "parsec",
        "parser-combinators",
        "parsers",
        "path",
        "path-io",
        "pipes",
        "pretty",
        "prettyprinter",
        "prettyprinter-ansi-terminal",
        "pretty-show",
        "process",
        "proto3-suite",
        "proto3-wire",
        "QuickCheck",
        "quickcheck-instances",
        "random",
        "range-set-list",
        "recursion-schemes",
        "regex-tdfa",
        "regex-tdfa-text",
        "retry",
        "rope-utf16-splay",
        "safe",
        "safe-exceptions",
        "scientific",
        "semigroups",
        "semver",
        "shake",
        "sorted-list",
        "split",
        "stache",
        "stm",
        "swagger2",
        "syb",
        "system-filepath",
        "tagged",
        "tar",
        "tar-conduit",
        "tasty",
        "tasty-ant-xml",
        "tasty-golden",
        "tasty-hunit",
        "tasty-quickcheck",
        "template-haskell",
        "temporary",
        "terminal-progress-bar",
        "text",
        "time",
        "tls",
        "transformers",
        "transformers-base",
        "turtle",
        "typed-process",
        "uniplate",
        "unix-compat",
        "unliftio",
        "unliftio-core",
        "unordered-containers",
        "uri-encode",
        "utf8-string",
        "uuid",
        "vector",
        "xml",
        "xml-conduit",
        "yaml",
        "zip",
        "zip-archive",
        "zlib",
        "zlib-bindings",
    ] + (["unix"] if not is_windows else ["Win32"]),
    tools = [
        "@alex",
        "@c2hs",
        "@happy",
    ],
    vendored_packages = {
        "ghcide": "@ghcide_ghc_lib//:ghcide",
        "grpc-haskell-core": "@grpc_haskell_core//:grpc-haskell-core",
    },
)

# Used to bootstrap `@c2hs` for `@stackage`.
stack_snapshot(
    name = "c2hs_deps",
    local_snapshot = "//:stack-snapshot.yaml",
    packages = [
        "base",
        "bytestring",
        "dlist",
        "filepath",
        "language-c",
    ],
    tools = [
        "@alex",
        "@happy",
    ],
)

register_toolchains(
    "//:c2hs-toolchain",
)

load("//bazel_tools/dev_env_package:dev_env_package.bzl", "dev_env_package")
load("//bazel_tools/dev_env_package:dev_env_tool.bzl", "dev_env_tool")
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load("//bazel_tools:ghc_dwarf.bzl", "ghc_dwarf")

ghc_dwarf(name = "ghc_dwarf")

load("@ghc_dwarf//:ghc_dwarf.bzl", "enable_ghc_dwarf")

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//nix:nixpkgs.nix",
    nix_file_deps = [
        "//nix:nixpkgs/default.nix",
        "//nix:nixpkgs/default.src.json",
    ],
)

dev_env_nix_repos = {
    "nixpkgs": "@nixpkgs",
}

# Bazel cannot automatically determine which files a Nix target depends on.
# rules_nixpkgs offers the nix_file_deps attribute for that purpose. It should
# list all files that a target depends on. This allows Bazel to rebuild the
# target using Nix if any of these files has been changed. Omitting files from
# this list can cause subtle bugs or cache misses when Bazel loads an outdated
# store path. You can use the following command to determine what files a Nix
# target depends on. E.g. for tools.curl
#
# $ nix-build -vv -A tools.curl nix 2>&1 \
#     | egrep '(evaluating file|copied source)' \
#     | egrep -v '/nix/store'
#
# Unfortunately there is no mechanism to automatically keep this list up to
# date at the moment. See https://github.com/tweag/rules_nixpkgs/issues/74.
common_nix_file_deps = [
    "//nix:bazel.nix",
    "//nix:nixpkgs.nix",
    "//nix:nixpkgs/default.nix",
    "//nix:nixpkgs/default.src.json",
]

# Use Nix provisioned cc toolchain
nixpkgs_cc_configure(
    nix_file = "//nix:bazel-cc-toolchain.nix",
    nix_file_deps = common_nix_file_deps + [
        "//nix:bazel-cc-toolchain.nix",
        "//nix:tools/bazel-cc-toolchain/default.nix",
    ],
    repositories = dev_env_nix_repos,
)

# Curl system dependency
nixpkgs_package(
    name = "curl_nix",
    attribute_path = "curl",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

# Patchelf system dependency
nixpkgs_package(
    name = "patchelf_nix",
    attribute_path = "patchelf",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

# Tar & gzip dependency
nixpkgs_package(
    name = "tar_nix",
    attribute_path = "gnutar",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

dev_env_tool(
    name = "tar_dev_env",
    nix_include = ["bin/tar"],
    nix_label = "@tar_nix",
    nix_paths = ["bin/tar"],
    tools = ["tar"],
    win_include = ["usr/bin/tar.exe"],
    win_paths = ["usr/bin/tar.exe"],
    win_tool = "msys2",
)

nixpkgs_package(
    name = "gzip_nix",
    attribute_path = "gzip",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

dev_env_tool(
    name = "gzip_dev_env",
    nix_include = ["bin/gzip"],
    nix_label = "@gzip_nix",
    nix_paths = ["bin/gzip"],
    tools = ["gzip"],
    win_include = ["usr/bin/gzip.exe"],
    win_paths = ["usr/bin/gzip.exe"],
    win_tool = "msys2",
)

dev_env_tool(
    name = "mvn_dev_env",
    nix_include = ["bin/mvn"],
    nix_label = "@mvn_nix",
    nix_paths = ["bin/mvn"],
    tools = ["mvn"],
    win_include = [
        "bin",
        "boot",
        "conf",
        "lib",
    ],
    win_paths = ["bin/mvn"],
    win_tool = "maven-3.6.1",
)

nixpkgs_package(
    name = "awk_nix",
    attribute_path = "gawk",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

nixpkgs_package(
    name = "hlint_nix",
    attribute_path = "hlint",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps + [
        "//nix:overrides/hlint-2.1.15.nix",
        "//nix:overrides/haskell-src-exts-1.21.0.nix",
    ],
    repositories = dev_env_nix_repos,
)

nixpkgs_package(
    name = "zip_nix",
    attribute_path = "zip",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

dev_env_tool(
    name = "zip_dev_env",
    nix_include = ["bin/zip"],
    nix_label = "@zip_nix",
    nix_paths = ["bin/zip"],
    tools = ["zip"],
    win_include = ["usr/bin/zip.exe"],
    win_paths = ["usr/bin/zip.exe"],
    win_tool = "msys2",
)

load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)
load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)

nixpkgs_package(
    name = "glibc_locales",
    attribute_path = "glibcLocales",
    build_file_content = """
package(default_visibility = ["//visibility:public"])
filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
""",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
) if is_linux else None

nix_ghc_deps = common_nix_file_deps + [
    "//nix:ghc.nix",
    "//nix:with-packages-wrapper.nix",
    "//nix:overrides/ghc-8.6.5.nix",
    "//nix:overrides/ghc-8.6.3-binary.nix",
]

# This is used to get ghc-pkg on Linux.
nixpkgs_package(
    name = "ghc_nix",
    attribute_path = "ghc.ghc",
    build_file_content = """
package(default_visibility = ["//visibility:public"])
exports_files(glob(["lib/**/*"]))
""",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = nix_ghc_deps,
    repositories = dev_env_nix_repos,
) if not is_windows else None

common_ghc_flags = [
    # We default to -c opt but we also want -O1 in -c dbg builds
    # since we use them for profiling.
    "-O1",
    "-hide-package=ghc-boot-th",
    "-hide-package=ghc-boot",
]

# Used by Darwin and Linux
haskell_register_ghc_nixpkgs(
    attribute_path = "ghcStaticDwarf" if enable_ghc_dwarf else "ghcStatic",
    build_file = "@io_tweag_rules_nixpkgs//nixpkgs:BUILD.pkg",

    # -fexternal-dynamic-refs is required so that we produce position-independent
    # relocations against some functions (-fPIC alone isn’t sufficient).

    # -split-sections would allow us to produce significantly smaller binaries, e.g., for damlc,
    # the binary shrinks from 186MB to 83MB. -split-sections only works on Linux but
    # we get a similar behavior on Darwin by default.
    # However, we had to disable split-sections for now as it seems to interact very badly
    # with the GHCi linker to the point where :main takes several minutes rather than several seconds.
    compiler_flags = common_ghc_flags + [
        "-fexternal-dynamic-refs",
    ] + (["-g3"] if enable_ghc_dwarf else []),
    compiler_flags_select = {
        "@com_github_digital_asset_daml//:profiling_build": ["-fprof-auto"],
        "//conditions:default": [],
    },
    is_static = True,
    locale_archive = "@glibc_locales//:locale-archive",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = nix_ghc_deps,
    repl_ghci_args = [
        "-O0",
        "-fexternal-interpreter",
        "-Wwarn",
    ],
    repositories = dev_env_nix_repos,
    version = "8.6.5",
)

# Used by Windows
haskell_register_ghc_bindists(
    compiler_flags = common_ghc_flags,
    version = "8.6.5",
) if is_windows else None

nixpkgs_package(
    name = "jq",
    attribute_path = "jq",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

dev_env_tool(
    name = "jq_dev_env",
    nix_include = ["bin/jq"],
    nix_label = "@jq",
    nix_paths = ["bin/jq"],
    tools = ["jq"],
    win_include = ["mingw64/bin"],
    win_include_as = {"mingw64/bin": "bin"},
    win_paths = ["bin/jq.exe"],
    win_tool = "msys2",
)

nixpkgs_package(
    name = "mvn_nix",
    attribute_path = "mvn",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

#node & npm
nixpkgs_package(
    name = "node_nix",
    attribute_path = "nodejs",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

nixpkgs_package(
    name = "npm_nix",
    attribute_path = "nodejs",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

#sass
nixpkgs_package(
    name = "sass_nix",
    attribute_path = "sass",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps + [
        "//nix:overrides/sass/default.nix",
        "//nix:overrides/sass/Gemfile",
        "//nix:overrides/sass/Gemfile.lock",
        "//nix:overrides/sass/gemset.nix",
    ],
    repositories = dev_env_nix_repos,
)

#tex
nixpkgs_package(
    name = "texlive_nix",
    attribute_path = "texlive",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

#sphinx
nixpkgs_package(
    name = "sphinx_nix",
    attribute_path = "sphinx183",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps + [
        "//nix:tools/sphinx183/default.nix",
    ],
    repositories = dev_env_nix_repos,
)

#Imagemagick
nixpkgs_package(
    name = "imagemagick_nix",
    attribute_path = "imagemagick",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

#Docker
nixpkgs_package(
    name = "docker_nix",
    attribute_path = "docker",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

#Javadoc
nixpkgs_package(
    name = "jdk_nix",
    attribute_path = "jdk8",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

# This will not be needed after merge of the PR to bazel adding proper javadoc filegroups:
# https://github.com/bazelbuild/bazel/pull/7898
# `@javadoc_dev_env//:javadoc` could be then replaced with `@local_jdk//:javadoc` and the below removed
dev_env_tool(
    name = "javadoc_dev_env",
    nix_include = ["bin/javadoc"],
    nix_label = "@jdk_nix",
    nix_paths = ["bin/javadoc"],
    tools = ["javadoc"],
    win_include = [
        "bin",
        "include",
        "jre",
        "lib",
    ],
    win_paths = ["bin/javadoc.exe"],
    win_tool = "java-openjdk-8u201",
)

# This only makes sense on Windows so we just put dummy values in the nix fields.
dev_env_tool(
    name = "makensis_dev_env",
    nix_include = [""],
    nix_paths = ["bin/makensis.exe"],
    tools = ["makensis"],
    win_include = [
        "bin",
        "contrib",
        "include",
        "plugins",
        "stubs",
    ],
    win_paths = ["bin/makensis.exe"],
    win_tool = "nsis-3.04",
) if is_windows else None

# Scaladoc
nixpkgs_package(
    name = "scala_nix",
    attribute_path = "scala",
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

# Dummy target //external:python_headers.
# To avoid query errors due to com_google_protobuf.
# See https://github.com/protocolbuffers/protobuf/blob/d9ccd0c0e6bbda9bf4476088eeb46b02d7dcd327/util/python/BUILD
bind(
    name = "python_headers",
    actual = "@com_google_protobuf//util/python:python_headers",
)

http_archive(
    name = "static_asset_d3plus",
    build_file_content = 'exports_files(["js/d3.min.js", "js/d3plus.min.js"])',
    sha256 = "7d31a500a4850364a966ac938eea7f2fa5ce1334966b52729079490636e7049a",
    strip_prefix = "d3plus.v1.9.8",
    type = "zip",
    urls = ["https://github.com/alexandersimoes/d3plus/releases/download/v1.9.8/d3plus.zip"],
)

load("//bazel_tools:java.bzl", "java_home_runtime")

java_home_runtime(name = "java_home")

# rules_go used here to compile a wrapper around the protoc-gen-scala plugin
load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

nixpkgs_package(
    name = "go_nix",
    attribute_path = "go",
    build_file_content = """
    filegroup(
        name = "sdk",
        srcs = glob(["share/go/**"]),
        visibility = ["//visibility:public"],
    )
    """,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

# A repository that generates the Go SDK imports, see
# ./bazel_tools/go_sdk/README.md
local_repository(
    name = "go_sdk_repo",
    path = "bazel_tools/go_sdk",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_wrap_sdk")

# On Nix platforms we use the Nix provided Go SDK, on Windows we let Bazel pull
# an upstream one.
go_wrap_sdk(
    name = "go_sdk",
    root_file = "@go_nix//:share/go/ROOT",
) if not is_windows else None

go_rules_dependencies()

go_register_toolchains()

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies", "go_repository")

gazelle_dependencies()

# protoc-gen-doc repo
go_repository(
    name = "com_github_pseudomuto_protoc_gen_doc",
    commit = "0c4d666cfe1175663cf067963396a0b9b34f543f",
    importpath = "github.com/pseudomuto/protoc-gen-doc",
)

# protokit repo
go_repository(
    name = "com_github_pseudomuto_protokit",
    commit = "7037620bf27b13fcdc10b1b17ddef82540db670b",
    importpath = "github.com/pseudomuto/protokit",
)

load(
    "@io_bazel_rules_scala//scala:scala.bzl",
    "scala_repositories",
)

scala_repositories((
    "2.12.6",
    {
        "scala_compiler": "3023b07cc02f2b0217b2c04f8e636b396130b3a8544a8dfad498a19c3e57a863",
        "scala_library": "f81d7144f0ce1b8123335b72ba39003c4be2870767aca15dd0888ba3dab65e98",
        "scala_reflect": "ffa70d522fc9f9deec14358aa674e6dd75c9dfa39d4668ef15bb52f002ce99fa",
    },
))

load("@io_bazel_rules_scala//scala:toolchains.bzl", "scala_register_toolchains")

scala_register_toolchains()

load("@io_bazel_rules_scala//jmh:jmh.bzl", "jmh_repositories")

jmh_repositories()

dev_env_package(
    name = "nodejs_dev_env",
    nix_label = "@node_nix",
    symlink_path = "nodejs_dev_env",
    win_tool = "nodejs-10.12.0",
)

# Setup the Node.js toolchain
load("@build_bazel_rules_nodejs//:defs.bzl", "node_repositories", "yarn_install")

node_repositories(
    package_json = ["//:package.json"],
    vendored_node = "@nodejs_dev_env",
)

yarn_install(
    name = "npm",
    package_json = "//:package.json",
    yarn_lock = "//:yarn.lock",
)

# Install all Bazel dependencies of the @npm packages
load("@npm//:install_bazel_dependencies.bzl", "install_bazel_dependencies")

install_bazel_dependencies()

load("@npm_bazel_typescript//:defs.bzl", "ts_setup_workspace")

ts_setup_workspace()

# TODO use fine-grained managed dependency
yarn_install(
    name = "daml_extension_deps",
    package_json = "//compiler/daml-extension:package.json",
    yarn_lock = "//compiler/daml-extension:yarn.lock",
)

# TODO use fine-grained managed dependency
yarn_install(
    name = "navigator_frontend_deps",
    package_json = "//navigator/frontend:package.json",
    yarn_lock = "//navigator/frontend:yarn.lock",
)

# Bazel Skydoc - Build rule documentation generator
load("@io_bazel_rules_sass//:package.bzl", "rules_sass_dependencies")

rules_sass_dependencies()

load("@io_bazel_rules_sass//:defs.bzl", "sass_repositories")

sass_repositories()

load("//3rdparty:workspace.bzl", "maven_dependencies")

maven_dependencies()

load("@io_bazel_skydoc//skylark:skylark.bzl", "skydoc_repositories")

skydoc_repositories()

# We usually use the _deploy_jar target to produce self-contained jars, but here we're using jar_jar because the size
# of codegen tool is substantially reduced (as shown below) and that the presence of JVM internal com.sun classes could
# theoretically stop the codegen running against JVMs other the OpenJDK 8 (the current JVM used for building).
load("@com_github_johnynek_bazel_jar_jar//:jar_jar.bzl", "jar_jar_repositories")

jar_jar_repositories()

# The following is advertised by rules_proto, but we define our own dependencies
# in dependencies.yaml. So all we need to do is replicate the binds here
# https://github.com/stackb/rules_proto/tree/master/java#java_grpc_library

# load("@io_grpc_grpc_java//:repositories.bzl", "grpc_java_repositories")
# grpc_java_repositories()

# Load the grpc deps last, since it won't try to load already loaded
# dependencies.
load("@com_github_grpc_grpc//bazel:grpc_deps.bzl", "grpc_deps")

grpc_deps()

load("@upb//bazel:workspace_deps.bzl", "upb_deps")

upb_deps()

load("@build_bazel_rules_apple//apple:repositories.bzl", "apple_rules_dependencies")

apple_rules_dependencies()

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

nixpkgs_package(
    name = "python3_nix",
    attribute_path = "python3",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

register_toolchains("//:nix_python_toolchain") if not is_windows else None

nixpkgs_package(
    name = "postgresql_nix",
    attribute_path = "postgresql",
    fail_not_supported = False,
    nix_file = "//nix:bazel.nix",
    nix_file_deps = common_nix_file_deps,
    repositories = dev_env_nix_repos,
)

dev_env_tool(
    name = "postgresql_dev_env",
    nix_include = [
        "bin",
        "include",
        "lib",
        "share",
    ],
    nix_label = "@postgresql_nix",
    nix_paths = [
        "bin/initdb",
        "bin/createdb",
        "bin/pg_ctl",
        "bin/postgres",
    ],
    tools = [
        "createdb",
        "initdb",
        "pg_ctl",
        "postgresql",
    ],
    win_include = [
        "mingw64/bin",
        "mingw64/include",
        "mingw64/lib",
        "mingw64/share",
    ],
    win_include_as = {
        "mingw64/bin": "bin",
        "mingw64/include": "include",
        "mingw64/lib": "lib",
        "mingw64/share": "share",
    },
    win_paths = [
        "bin/initdb.exe",
        "bin/createdb.exe",
        "bin/pg_ctl.exe",
        "bin/postgresql.exe",
    ],
    win_tool = "msys2",
)
