---
linkTitle: "Updates"
title: "Reference: updates"
weight: 4
description: >
  This page gives reference information on Updates.
---

For the structure around them, see [Overview: template structure]({{< relref "_index.md" >}}).

## Background

- An `Update` is a ledger update. There are many different kinds of
these, and they're listed below.
- They are what can go in a [choice body]({{< relref "choices.md#choice-body" >}}).

## Binding variables

{{< highlight haskell >}}
boundVariable <- UpdateExpression1
{{< / highlight >}}

- One of the things you can do in a choice body is bind (assign) an
Update expression to a variable. This works for any of the Updates
below.

## do

{{< highlight haskell >}}
do
   updateExpression1
   updateExpression2
{{< / highlight >}}


- `do` can be used to group `Update` expressions. You can only have
one update expression in a choice, so any choice beyond the very
simple will use a `do` block.
- Anything you can put into a choice body, you can put into a `do`
block.
- By default, `do` returns whatever is returned by the **last
expression in the block**.

	So if you want to return something else, you'll need to use `return`
	explicitly - see [Return]({{< relref "#return" >}}) for an example.

## create

{{< highlight haskell >}}
create NameOfTemplate with exampleParameters
{{< / highlight >}}

- `create` function.
- Creates an instance of that contract on the ledger. When a contract
is committed to the ledger, it is given a unique contract identifier
of type `ContractId <name of template>`.

	Creating the contract returns that `ContractId`.
- Use `with` to specify the template parameters.
- Requires authorization from the signatories of the contract being
created. This is given by being signatories of the contract from
which the other contract is created, being the controller, or
explicitly creating the contract itself.

	If the required authorization is not given, the transaction fails.
	For more detail on authorization, see [Signatories]({{< relref "templates.md#signatories" >}}).

## exercise

{{< highlight haskell >}}
exercise IdOfContract NameOfChoiceOnContract with choiceArgument1 = value1
{{< / highlight >}}

- `exercise` function.
- Exercises the specified choice on the specified contract.
- Use `with` to specify the choice parameters.
- Requires authorization from the controller(s) of the choice. If the
authorization is not given, the transaction fails.

## exerciseByKey

{{< highlight haskell >}}
exercise exerciseByKey @ContractType contractKey NameofChoiceOnContract with choiceArgument1 = value1
{{< / highlight >}}


- `exerciseByKey` function.
- Exercises the specified choice on the specified contract.
- Use `with` to specify the choice parameters.
- Requires authorization from the controller(s) of the choice **and**
from at least one of the maintainers of the key. If the
authorization is not given, the transaction fails.

## fetch

{{< highlight haskell >}}
fetchedContract <- fetch IdOfContract
{{< / highlight >}}

- `fetch` function.
- Fetches the contract instance with that ID. Usually used with a
bound variable, as in the example above.
- Often used to check the details of a contract before exercising a
choice on that contract. Also used when referring to some reference
data.
- `fetch cid` fails if `cid` is not the contract id of an active
contract, and thus causes the entire transaction to abort.
- The submitting party must be an observer or signatory on the
contract, otherwise `fetch` fails, and similarly causes the entire
transaction to abort.

## fetchByKey

{{< highlight haskell >}}
fetchedContract <- fetchByKey @ContractType contractKey
{{< / highlight >}}

- `fetchByKey` function.
- The same as `fetch`, but fetches the contract instance with that
[contract key]({{< relref "contract-keys.md" >}}), instead of the
contract ID.
- As well as the authorization that `fetch` requires, you also need
authorization from one of the `maintainers` of the key.

## lookupByKey

{{< highlight haskell >}}
fetchedContractId <- lookupByKey @ContractType contractKey
{{< / highlight >}}

- `lookupByKey` function.
- Use this to confirm that a contract with the given [contract key]({{< relref "contract-keys.md" >}}) exists.
- If it does exist, `lookupByKey` returns the `ContractId` of the
contract; otherwise, it returns `None`. If it returns `None`, this
guarantees that no contract has this key. This does **not** cause
the transaction to abort.
- **All** of the maintainers of the key must authorize the lookup (by
either being signatories or by submitting the command to lookup),
otherwise this will fail.

## abort

{{< highlight haskell >}}
abort errorMessage
{{< / highlight >}}

- `abort` function.
- Fails the transaction - nothing in it will be committed to the
ledger.
- `errorMessage` is of type `Text`. Use the error message to provide
more context to an external system (e.g., it gets displayed in DAML
Studio scenario results).
- You could use `assert False` as an alternative.

## assert

{{< highlight haskell >}}
assert (condition == True)
{{< / highlight >}}

- `assert` keyword.
- Fails the transaction if the condition is false. So the choice can
only be exercised if the boolean expression evaluates to `True`.
- Often used to restrict the arguments that can be supplied to a
contract choice.

Here's an example of using `assert` to prevent a choice being exercised
if the `Party` passed as a parameter is on a blacklist:

TODO snippet

## getTime

{{< highlight haskell >}}
currentTime <- getTime
{{< / highlight >}}

- `getTime` keyword.
- Gets the ledger effective time. (You will usually want to
immediately bind it to a variable in order to be able to access the
value.)
- Used to restrict when a choice can be made. For example, with an
`assert` that the time is later than a certain time.

Here's an example of a choice that uses a check on the current time:

TODO snippet

## return

{{< highlight haskell >}}
return ()
{{< / highlight >}}

- `return` keyword.
- Used to return a value from `do` block that is not of type `Update`.

Here's an example where two contracts are created in a choice and both
their ids are returned as a tuple:

{{< highlight haskell >}}
do
  firstContract <- create SomeContractTemplate with arg1; arg2
  secondContract <- create SomeContractTemplate with arg1; arg2
  return (firstContract, secondContract)
{{< / highlight >}}

## let

See the documentation on `daml-ref-let`.

Let looks similar to binding variables, but it's very different\! This
code example shows how:

{{< highlight haskell >}}
do
  -- defines a function, createdContract, taking a single argument that when
  -- called _will_ create the new contract using argument for issuer and owner
  let createContract x = create NameOfContract with issuer = x; owner = x

  createContract party1
  createContract party2
{{< / highlight >}}

## this

`this` lets you refer to the current contract from within the choice
body. This refers to the contract, *not* the contract ID.

It's useful, for example, if you want to pass the current contract to a
helper function outside the template.

