---
title: "7 Composing choices"
weight: 8
---

# 7 Composing choices

It's time to put everything you've learnt so far together into a
complete and secure DAML model for asset issuance, management, transfer,
and trading. This application will have capabilities similar to the one
in the [Quickstart]({{< relref "quickstart.md" >}}). In the process you will learn about a
few more concepts:

- DAML projects, packages and modules
- Composition of transactions
- Observers and stakeholders
- DAML's execution model
- Privacy

The model in this section is not a single DAML file, but a DAML project
consisting of several files that depend on each other.

## DAML projects

DAML is organized in packages and modules. A DAML project is specified
using a single `daml.yaml` file, and compiles into a package. Each DAML
file within a project becomes a DAML module. You can start a new project
with a skeleton structure using `daml new project_name` in the terminal.

Each DAML project has a main source file, which is the entry point for
the compiler. A common pattern is to have a main file called
`LibraryModules.daml`, which simply lists all the other modules to
include.

A minimal project would contain just two files: `daml.yaml` and
`daml/LibraryModules.daml`. Take a look at the `daml.yaml` for this
project:

TODO rest of file.

## Project structure

## Project overview

## Composed choices and scenarios

## DAML's execution model

## Observers

## Privacy

### Divulgence
