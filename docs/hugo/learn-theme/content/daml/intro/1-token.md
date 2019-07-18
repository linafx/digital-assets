---
title: "1 Basic contracts"
weight: 2
---

To begin with, you're going to write a very small DAML template, which
represents a self-issued, non-transferrable token. Because it's a
minimal template, it isn't actually useful on its own - you'll make it
more useful later - but it's enough that it can show you the most basic
concepts:

- Transactions
- DAML Modules and Files
- Templates
- Contracts
- Signatories

## DAML ledger basics

Like most structures called ledgers, a DAML Ledger is just a list of
*commits*. When we say *commit*, we mean the final result of when a
*party* successfully *submits* a *transaction* to the ledger.

*Transaction* is a concept we'll cover in more detail through this
introduction. The most basic examples are the creation and archival of a
*contract*.

A contract is *active* from the point where there is a committed
transaction that creates it, up to the point where there is a committed
transaction that *archives* it again.

DAML specifies what transactions are legal on a DAML Ledger. The rules
the DAML code specifies are collectively called a *DAML model* or
*contract model*.

## DAML files and modules

Each `.daml` file defines a *DAML Module*. At the top of each DAML file
is a pragma informing the compiler of the language version and the
module name:

TODO the rest.

## DAML files and modules

## Templates

## Signatories

## Next up

In [2 Testing templates using scenarios]({{< relref "2-scenario.md" >}}),
you'll learn about how to try out the `Token` contract
template in DAML's inbuilt `scenario` testing language.
