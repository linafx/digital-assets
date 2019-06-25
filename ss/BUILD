load("//bazel_tools:haskell.bzl", "da_haskell_binary")

da_haskell_binary(
    name = "run",
    srcs = ["Main.hs"],
    hazel_deps = [
        "base",
        "extra",
        "text",
        "managed",
    ],
    deps = [
        "//nix/third-party/gRPC-haskell:grpc-haskell",
        "//nix/third-party/gRPC-haskell/core:grpc-haskell-core",
        "//daml-foundations/daml-ghc/daml-compiler",
        "//compiler/haskell-ide-core",
        "//compiler/scenario-service/client",
        "//libs-haskell/da-hs-base",
    ],
    data = [
        "//compiler/scenario-service/server:scenario_service_jar",
        "//daml-foundations/daml-ghc/package-database:package-db",
    ],
)
