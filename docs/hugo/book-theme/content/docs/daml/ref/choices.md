---
linkTitle: "Choices"
title: "Reference: choices"
weight: 3
description: >
  This page gives reference information on choices.
---

For information on the high-level structure of a choice, see
[Overview: template structure]({{< relref "_index.md" >}}).

## `choice` first or `controller` first

There are two ways you can start a choice:

- start with the `choice` keyword
- start with the `controller` keyword

TODO snippet

The main difference is that starting with `choice` means that you can
pass in a `Party` to use as a controller. If you do this, you **must**
make sure that you add that party as an `observer`, otherwise they won't
be able to see the contract (and therefore won't be able to exercise the
choice).

In contrast, if you start with `controller`, the `controller` is
automatically added as an observer when you compile your DAML files.

## Choice name

## Controllers

### Non-consuming choices

### Return type

## Choice arguments

## Choice body
