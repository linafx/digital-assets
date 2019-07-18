---
linkTitle: "Scenarios"
title: "Reference: scenarios"
weight: 8
description: >
  This page gives reference information on scenario syntax,
  used for testing templates.
---

For an introduction to scenarios, see [Scenarios]({{< relref "testing.md" >}}).

## Scenario keyword

- `scenario` function. Introduces a series of transactions to be
submitted to the ledger.

## Submit

- `submit` keyword.
- Submits an action (a create or an exercise) to the ledger.
- Takes two arguments, the party submitting followed by the
expression, for example: `submit bankOfEngland do create ...`

## submitMustFail

- `submitMustFail` keyword.
- Like `submit`, but you're asserting it should fail.
- Takes two arguments, the party submitting followed by the expression
by a party, for example: `submitMustFail bankOfEngland do create
...`

## Scenario body

### Updates

- Usually `create` and `exercise`. But you can also use other updates, like
`assert` and `fetch`.
- Parties can only be named explicitly in scenarios.

### Passing time

In a scenario, you may want time to pass so you can test something
properly. You can do this with `pass`.

Here's an example of passing time:

TODO snippet

### Binding variables

As in choices, you can `bind to variables`.
Usually, you'd bind commits to variables in order to get the returned
value (usually the contract).

