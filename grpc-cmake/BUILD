load("@rules_foreign_cc//tools/build_defs:cmake.bzl", "cmake_external")

cmake_external(
    name = "ares",
    # cache_entries = {
    #     "CARES_SHARED": "no",
    #     "CARES_STATIC": "on",
    # },
    # defines = ["CARES_STATICLIB"],
    lib_source = "@ares-src//:all",
    shared_libraries = ["libcares.so"],
    make_commands = ["ninja", "ninja install"],
    cmake_options = ["-GNinja"],
)

cmake_external(
   name = "cmake-grpc",
   lib_source = "@grpc-src//:all",
   static_libraries = ["libgrpc.a"],
   make_commands = [
       "ninja",
       "ninja install",
   ],
   cache_entries = {
     "gRPC_ZLIB_PROVIDER": "package",
     "gRPC_CARES_PROVIDER": "package",
     "gRPC_PROTOBUF_PROVIDER": "package",
     "CARES_DIR": "$EXT_BUILD_DEPS/ares",
#     "CARES_INCLUDE_DIR": "$EXT_BUILD_DEPS/ares/include",
   },
   cmake_options = ["-GNinja"],
   deps = ["@com_github_madler_zlib//:libz", ":ares", "@com_google_protobuf//:protobuf"],
)
