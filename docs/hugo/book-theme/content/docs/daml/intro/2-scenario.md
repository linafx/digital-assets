---
title: "2 Testing templates using scenarios"
weight: 3
---

# 2 Testing templates using scenarios

In this section you will test the `Token` model from
[Basic contracts]({{< relref "1-token.md" >}}) using
DAML's inbuilt `scenario` language. You'll learn about the basic
features of scenarios:

- Getting parties
- Submitting transactions
- Creating contracts
- Testing for failure
- Archiving contracts
- Viewing ledger and final ledger state

## Scenario basics

A `Scenario` is like a recipe for a test, where you can script different
parties submitting a series of transactions, to check that your
templates behave as you'd expect. You can also script some some external
information like party identities, and ledger time.

Below is a basic scenario that creates a `Token` for a party called
"Alice".

TODO rest of file.

## Running scenarios

## Testing for failure

## Archiving contracts

## Exploring the ledger

## Exercises

## Next up

In [3 Data types]({{< relref "3-data.md" >}}) you will learn about DAML's type system, and how you can think of templates as tables and contracts as database rows.