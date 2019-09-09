# Third-Party JVM Dependencies

This subtree contains a Bazel workspace file `workspace.bzl` and `BUILD` files
underneath `jvm/` describing external Java and Scala dependencies. These files
are automatically generated and should not be modified manually. If you need to
add a third party dependency then modify the file `dependencies.yaml` in the
repository root and re-run `bazel-deps` in order to regenerate these files as
described at the top of the file `dependencies.yaml`.
