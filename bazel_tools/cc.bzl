load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")

CcUnifiedLibrary = provider(
    fields = {
        "targets": "",
        "transitive_deps": "",
    },
)

def merge_CcUnifiedLibraries(infos = []):
    return CcUnifiedLibrary(
        targets = dicts.add(*[info.targets for info in infos]),
        transitive_deps = depset(transitive = [
            info.transitive_deps
            for info in infos
        ]),
    )

def _cc_unified_library_aspect_impl(target, ctx):
    if not CcInfo in target:
        return []

    objects = None
    if ctx.rule.kind == "cc_library" and CcInfo in target:
        cc_toolchain = find_cpp_toolchain(ctx)
        feature_configuration = cc_common.configure_features(
            ctx = ctx,
            cc_toolchain = cc_toolchain,
        )
        src_exts = ["cc", "cpp", "cxx", "c++", "C", "c"]
        hdr_exts = ["h", "hh", "hpp", "ipp", "hxx", "h++", "inc", "inl", "tlh", "tli", "H"]
        input_compilation_context = target[CcInfo].compilation_context
        (compilation_context, compilation_outputs) = cc_common.compile(
            actions = ctx.actions,
            feature_configuration = feature_configuration,
            cc_toolchain = cc_toolchain,
            srcs = [
                src for src in ctx.rule.files.srcs
                if src.extension in src_exts
            ],
            public_hdrs = [
                hdr for hdr in ctx.rule.files.hdrs
                if hdr.extension in hdr_exts
            ],
            private_hdrs = [
                hdr for hdr in ctx.rule.files.srcs
                if hdr.extension in hdr_exts
            ],
            includes = input_compilation_context.includes.to_list(),
            quote_includes = input_compilation_context.quote_includes.to_list(),
            system_includes = input_compilation_context.system_includes.to_list(),
            framework_includes = input_compilation_context.framework_includes.to_list(),
            defines = input_compilation_context.defines.to_list(),
            #local_defines = [],  # as of Bazel 0.29
            user_compile_flags = ctx.rule.attr.copts,  # XXX: Should be subject to make variable expansion
            compilation_contexts = [
                dep[CcInfo].compilation_context
                for dep in ctx.rule.attr.deps
                if CcInfo in dep
            ],
            name = target.label.name + "-unified",
            disallow_pic_outputs = False,
            disallow_nopic_outputs = False,
        )
        objects = struct(
            compilation_context = compilation_context,
            compilation_outputs = compilation_outputs,
        )

    transitive_deps = depset(
        direct = [target.label],
        transitive = [
            dep[CcUnifiedLibrary].transitive_deps
            for dep in ctx.rule.attr.deps
            if CcUnifiedLibrary in dep
        ],
    )

    info = CcUnifiedLibrary(
        targets = {
            target.label: struct(
                objects = objects,
                libraries = target[CcInfo],
                transitive_deps = transitive_deps,
            ),
        },
        transitive_deps = transitive_deps,
    )

    return [merge_CcUnifiedLibraries(infos = [info] + [
        dep[CcUnifiedLibrary]
        for dep in getattr(ctx.rule.attr, "deps", [])
        if CcUnifiedLibrary in dep
    ])]

cc_unified_library_aspect = aspect(
    _cc_unified_library_aspect_impl,
    attr_aspects = ["deps"],
    attrs = {
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    fragments = ["cpp"],
)

def _cc_unified_library_impl(ctx):
    cc_toolchain = find_cpp_toolchain(ctx)
    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
    )

    linking_contexts = [
        dep[CcInfo].linking_context
        for dep in ctx.attr.deps
        if CcInfo in dep
    ]

    info = merge_CcUnifiedLibraries(infos = [
        lib[CcUnifiedLibrary]
        for lib in ctx.attr.libs
    ])

    exclude = depset(
        direct = [
            target.label
            for target in ctx.attr.exclude
        ],
        transitive = [
            info.targets[target.label].transitive_deps
            for target in ctx.attr.exclude
            if target.label in info.targets
        ],
    )
    exclude = {k: () for k in exclude.to_list()}

    compilation_outputs_acc = []
    headers_acc = []
    system_includes_acc = []
    includes_acc = []
    quote_includes_acc = []
    framework_includes_acc = []
    defines_acc = []
    for (label, value) in info.targets.items():
        if label in exclude:
            continue
        compilation_outputs_acc.append(value.objects.compilation_outputs)
        headers_acc.append(value.objects.compilation_context.headers)
        system_includes_acc.append(value.objects.compilation_context.system_includes)
        includes_acc.append(value.objects.compilation_context.includes)
        quote_includes_acc.append(value.objects.compilation_context.quote_includes)
        framework_includes_acc.append(value.objects.compilation_context.framework_includes)
        defines_acc.append(value.objects.compilation_context.defines)

    compilation_outputs = cc_common.merge_compilation_outputs(compilation_outputs = compilation_outputs_acc)
    headers = depset(transitive = headers_acc)
    system_includes = depset(transitive = system_includes_acc)
    includes = depset(transitive = includes_acc)
    quote_includes = depset(transitive = quote_includes_acc)
    framework_includes = depset(transitive = framework_includes_acc)
    defines = depset(transitive = defines_acc)

    (linking_context, linking_outputs) = cc_common.create_linking_context_from_compilation_outputs(
        actions = ctx.actions,
        feature_configuration = feature_configuration,
        cc_toolchain = cc_toolchain,
        compilation_outputs = compilation_outputs,
        user_link_flags = [],  # XXX
        linking_contexts = linking_contexts,
        name = ctx.label.name,
        alwayslink = False,  # XXX
        #additional_inputs = [],  # XXX
        disallow_static_libraries = False,
        disallow_dynamic_library = False,
        #grep_includes = None,  # XXX: cc_library had an attribute like that
    )

    library_to_link = linking_outputs.library_to_link

    if not ctx.attr.mangle_name:
        replace_library_to_link = cc_common.create_library_to_link(
            actions = ctx.actions,
            feature_configuration = feature_configuration,
            cc_toolchain = cc_toolchain,
            static_library = library_to_link.static_library,
            pic_static_library = library_to_link.pic_static_library,
            dynamic_library = library_to_link.resolved_symlink_dynamic_library,
            interface_library = library_to_link.resolved_symlink_interface_library,
        )
        libraries_to_link = []
        for lib in linking_context.libraries_to_link.to_list():
            if lib != library_to_link:
                libraries_to_link.append(lib)
            else:
                libraries_to_link.append(replace_library_to_link)
        linking_context = cc_common.create_linking_context(
            libraries_to_link = libraries_to_link,
            user_link_flags = linking_context.user_link_flags,
            additional_inputs = linking_context.additional_inputs.to_list(),
        )
        library_to_link = replace_library_to_link

    cc_info = CcInfo(
        compilation_context = cc_common.create_compilation_context(
            headers = headers,
            system_includes = system_includes,
            includes = includes,
            quote_includes = quote_includes,
            framework_includes = framework_includes,
            defines = defines,
        ),
        linking_context = linking_context,
    )

    static_library = None
    if library_to_link.pic_static_library != None:
        static_library = library_to_link.pic_static_library 
    elif library_to_link.static_library != None:
        static_library = library_to_link.static_library 

    dynamic_library = None
    if library_to_link.resolved_symlink_dynamic_library != None:
        dynamic_library = library_to_link.resolved_symlink_dynamic_library
    elif library_to_link.resolved_symlink_interface_library != None:
        dynamic_library = library_to_link.resolved_symlink_interface_library

    default_info = DefaultInfo(files = depset(direct = [
        lib for lib in [static_library, dynamic_library] if lib != None
    ]))

    return [cc_info, default_info]


cc_unified_library = rule(
    _cc_unified_library_impl,
    attrs = {
        "libs": attr.label_list(
            aspects = [cc_unified_library_aspect],
            providers = [CcInfo],
        ),
        "exclude": attr.label_list(),
        "deps": attr.label_list(
            providers = [CcInfo],
        ),
        "mangle_name": attr.bool(default = True),
        "_cc_toolchain": attr.label(
            default = Label("@bazel_tools//tools/cpp:current_cc_toolchain"),
        ),
    },
    fragments = ["cpp"],
)
