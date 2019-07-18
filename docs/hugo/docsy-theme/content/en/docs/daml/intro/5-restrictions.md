---
title: "5 Adding constraints to a contract"
weight: 6
---

You will often want to constrain the data stored or the allowed data
transformations in your contract models. In this section, you will learn
about the two main mechanisms provided in DAML:

  - The `ensure` keyword.
  - The `assert`, `abort` and `error` keywords.

To make sense of the latter, you'll also learn more about the `Update`
and `Scenario` types and `do` blocks, which will be good preparation for
[7 Composing choices]({{< relref "7-composing.md" >}}), where you will use `do` blocks to compose choices into
complex transactions.

Lastly, you will learn about time on the ledger and in scenarios.

## Template preconditions

The first kind of restriction you may want to put on the contract model
are called *template pre-conditions*. These are simply restrictions on
the data that can be stored on a contract from that template.

Suppose, for example, that the `SimpleIou` contract from `simple_iou`
should only be able to store positive amounts. You can enforce this
using the `ensure` keyword:

TODO rest of file.

## Assertions and errors

## Time on DAML ledgers

### Time in scenarios

### Time on ledgers

## Actions and `do` blocks

### Pure expressions compared to Actions

### Actions and impurity

### Chaining up actions with `do` blocks

### Wrapping values in actions

## Failing actions

## A sample Action

## Next up

You can now specify a precise data and data-transformation model for DAML ledgers. In [6 Parties and authority]({{< relref "6-parties.md" >}}), you will learn how to properly involve multiple parties in contracts, how authority works in DAML, and how to build contract 