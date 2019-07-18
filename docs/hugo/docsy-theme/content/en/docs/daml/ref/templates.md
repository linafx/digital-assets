---
linkTitle: "Templates"
title: "Reference: templates"
weight: 2
description: >
  This page gives reference information on templates.
---

For the structure of a template, see [Overview: template structure]({{< relref "_index.md" >}}).


## Template name

<div class="literalinclude" data-language="daml" data-start-after="-- start template intro snippet" data-end-before="-- end template intro snippet">

../code-snippets/Reference.daml

</div>

  - This is the name of the template. It's preceded by `template`
    keyword. Must begin with a capital letter.
  - This is the highest level of nesting.
  - The name is used when `creating <daml-ref-create>` a contract
    instance of this template (usually, from within a choice).

## Template parameters

<div class="literalinclude" data-language="daml" data-start-after="-- start template params snippet" data-end-before="-- end template params snippet">

../code-snippets/Reference.daml

</div>

  - `with` keyword. The parameters are in the form of a `record type
    <daml-ref-record-types>`.
  - Passed in when `creating <daml-ref-create>` a contract instance from
    this template. These are then in scope inside the template body.
  - A template parameter can't have the same name as any `choice
    arguments <daml-ref-choice-arguments>` inside the template.
  - For all parties involved in the contract (whether they're a
    `signatory`, `observer`, or `controller`) you must pass them in as
    parameters to the contract, whether individually or as a list
    (`[Party]`).

## Signatory parties

<div class="literalinclude" data-language="daml" data-start-after="-- start template sigs snippet" data-end-before="-- end template sigs snippet">

../code-snippets/Reference.daml

</div>

  - `signatory` keyword. After `where`. Followed by at least one
    `Party`.

  - Signatories are the parties (see the `Party` type) who must consent
    to the creation of an instance of this contract. They are the
    parties who would be put into an *obligable position* when this
    contract is created.
    
    DAML won't let you put someone into an obligable position without
    their consent. So if the contract will cause obligations for a
    party, they *must* be a signatory. **If they haven't authorized it,
    you won't be able to create the contract.** In this situation, you
    may see errors like:
    
    `NameOfTemplate requires authorizers Party1,Party2,Party, but only
    Party1 were given.`

  - When a signatory consents to the contract creation, this means they
    also authorize the consequences of `choices <daml-ref-choices>` that
    can be exercised on this contract.

  - The contract instance is visible to all signatories (as well as the
    other stakeholders of the contract). That is, the compiler
    automatically adds signatories as observers.

  - You **must** have least one signatory per template. You can have
    many, either as a comma-separated list or reusing the keyword. You
    could pass in a list (of type `[Party]`).

## Observers

../code-snippets/Reference.daml

- `observer` keyword. After `where`. Followed by at least one `Party`.
- Observers are additional stakeholders, so the contract instance is
  visible to these parties (see the `Party` type).
- Optional. You can have many, either as a comma-separated list or
  reusing the keyword. You could pass in a list (of type `[Party]`).
- Use when a party needs visibility on a contract, or be informed or
  contract events, but is not a `signatory <daml-ref-signatories>` or
  `controller <daml-ref-controllers>`.
- If you start your choice with `choice` rather than `controller` (see
  `daml-ref-choices` below), you must make sure to add any potential
  controller as an observer. Otherwise, they will not be able to
  exercise the choice, because they won't be able to see the contract.

## Choices

../code-snippets/Reference.daml

- A right that the contract gives the controlling party. Can be
  *exercised*.

- This is essentially where all the logic of the template goes.

- By default, choices are *consuming*: that is, exercising the choice
  archives the contract, so no further choices can be exercised on it.
  You can make a choice non-consuming using the `nonconsuming`
  keyword.

- There are two ways of specifying a choice: start with the `choice`
  keyword or start with the `controller` keyword.
  
  Starting with `choice` lets you pass in a `Party` to use as a
  controller. But you must make sure to add that party as an
  `observer`.

- See `choices` for full reference information.

## Agreements

../code-snippets/Reference.daml

- `agreement` keyword, followed by text.

- Represents what the contract means in text. They're usually the
  boundary between on-ledger and off-ledger rights and obligations.

- Usually, they look like `agreement tx`, where `tx` is of type
  `Text`.
  
  You can use the built-in operator `show` to convert party names to a
  string, and concatenate with `<>` .

## Preconditions

../code-snippets/Reference.daml

- `ensure` keyword, followed by a boolean condition.
- Used on contract creation. `ensure` limits the values on parameters
  that can be passed to the contract: the contract can only be created
  if the boolean condition is true.

## Contract keys and maintainers

../code-snippets/Reference.daml

- `key` and `maintainer` keywords.

- This feature lets you specify a "key" that you can use to uniquely
  identify an instance of this contract template.

- If you specify a `key`, you must also specify a `maintainer`. This
  is a `Party` that will ensure the uniqueness of all the keys it is
  aware of.
  
  Because of this, the `key` must include the `maintainer` `Party` or
  parties (for example, as part of a tuple or record), and the
  `maintainer` must be a signatory or observer of the contract
  instance.

- For a full explanation, see `/daml/reference/contract-keys`.