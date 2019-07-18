---
linkTitle: "DAML reference"
title: "Overview: template structure"
weight: 20
description: >
  This page covers what a template looks like: what parts of a template there are, and where they go.
---

For the structure of a DAML file *outside* a template, see
[File structure]({{< relref "file-structure.md" >}}).

## Template outline structure

Here’s the structure of a DAML template:

{{< highlight haskell >}}
template NameOfTemplate
  with
    exampleParty : Party
    exampleParty2 : Party
    exampleParty3 : Party
    exampleParameter : Text
    -- more parameters here
  where
    signatory exampleParty
    observer exampleParty2
    agreement
      -- some text
      ""
    ensure
      -- boolean condition
      True
    key (exampleParty, exampleParameter) : (Party, Text)
    maintainer (exampleFunction key)
    -- a choice goes here; see next section
{{< / highlight >}}

- [Template name]({{< relref "templates.md#template-name" >}}):
  `template` keyword

- [Parameters]({{< relref "templates.md#template-parameters" >}}):
  `with` followed by the names of parameters and their types

- Template body:
  `where` keyword
  
    Can include:
  
    - [Signatories]({{< relref "templates.md#signatory-parties" >}}):
      `signatory` keyword
      
        Required. The parties (see the `Party <daml-ref-built-in-types>`
        type) who must consent to the creation of an instance of this
        contract. You won't be able to create an instance of this
        contract until all of these parties have authorized it.

    - [observers]({{< relref "templates.md#observers" >}}):
      `observer` keyword
      
        Optional. Parties that aren't signatories but who you still want
        to be able to see this contract.

    - [an agreement]({{< relref "templates.md#agreements" >}}):
      `agreement` keyword
      
        Optional. Text that describes the agreement that this contract
        represents.

    - [a precondition]({{< relref "templates.md#preconditions" >}}): 
      `ensure` keyword
      
        Only create the contract if the conditions after `ensure`
        evaluate to true.

    - [a contract key]({{< relref "templates.md#contract-keys" >}}): 
      `key` keyword
      
        Optional. Lets you specify a combination of a party and other
        data that uniquely identifies an instance of this contract
        template. See [Contract keys]({{< relref "contract-keys.md" >}}).

    - [maintainers]({{< relref "templates.md#contract-keys" >}}): 
      `maintainer` keyword
      
        Required if you have specified a `key`. Keys are only unique to
        a `maintainer`. See [Contract keys]({{< relref "contract-keys.md" >}}).

    - [choices](#choice-structure) 

        `choice NameOfChoice : ReturnType controller nameOfParty do`
        
        or
        
        `controller nameOfParty can NameOfChoice : ReturnType do`
        
        Defines choices that can be exercised. See [Choice
        structure](#choice-structure) for what can go in a choice.

## Choice structure

Here's the structure of a choice inside a template. There are two ways
of specifying a choice:

- start with the `choice` keyword
- start with the `controller` keyword

{{< highlight haskell >}}
-- option 1 for specifying choices: choice name first
choice NameOfChoice :
      () -- replace () with the actual return type
    with
    party : Party -- parameters here
  controller party
    do
      return () -- replace this line with the choice body

-- option 2 for specifying choices: controller first
controller exampleParty can
  NameOfAnotherChoice :
      () -- replace () with the actual return type
    with
      party : Party -- parameters here
    do
      return () -- replace the line with the choice body
{{< / highlight >}}

- `a controller (or controllers) <daml-ref-controllers>`: 
  `controller` keyword
  
    Who can exercise the choice.

- `consumability <daml-ref-anytime>` :
  `nonconsuming` keyword
  
    By default, contracts are archived when a choice on them is
    exercised, which means that choices can no longer be exercised on
    them. If you include `nonconsuming`, this choice can be exercised
    over and over.

- `a name <daml-ref-choice-name>`

    Must begin with a capital letter. Must be unique - choices in
    different templates can't have the same name.

- `a return type <daml-ref-return-type>`  

    after a `:`, the return type of the choice

- `choice arguments <daml-ref-choice-arguments>`: 
  `with` keyword
  
    If you start your choice with `choice` and include a `Party` as a
    parameter, you can make that `Party` the `controller` of the choice.
    This is a feature called "flexible controllers", and it means you
    don't have to specify the controller when you create the contract -
    you can specify it when you exercise the choice. To exercise a
    choice, the party needs to be a signatory or an observer of the
    contract and must be explicitly declared as such.

- `a choice body <daml-ref-choice-body>`  
  After `do` keyword
  
    What happens when someone exercises the choice. A choice body can
    contain update statements: see [Choice body
    structure](#choice-body-structure) below.

## Choice body structure

A choice body contains `Update` expressions, wrapped in a `do
<daml-ref-do>` block.

The update expressions are:

- `create <daml-ref-create>`  

    Create a new contract instance of this template.
    
    `create NameOfContract with contractArgument1 = value1;
    contractArgument2 = value2; ...`

- `exercise <daml-ref-exercise>`  

    Exercise a choice on a particular contract.
    
    `exercise idOfContract NameOfChoiceOnContract with choiceArgument1 =
    value1; choiceArgument2 = value 2; ...`

- `fetch <daml-ref-fetch>`  

    Fetch a contract instance using its ID. Often used with assert to
    check conditions on the contract’s content.
    
    `fetchedContract <- fetch IdOfContract`

- `fetchByKey <daml-ref-fetch-by-key>`  

    Like `fetch`, but uses a `contract key
    </daml/reference/contract-keys>` rather than an ID.
    
    `fetchedContract <- fetchByKey @ContractType contractKey`

- `lookupByKey <daml-ref-lookup-by-key>`  

    Confirm that a contract with the given `contract key
    </daml/reference/contract-keys>` exists.
    
    `fetchedContractId <- lookupByKey @ContractType contractKey`

- `abort <daml-ref-abort>`  

    Stop execution of the choice, fail the update.
    
    `if False then abort`

- `assert <daml-ref-assert>`  

    Fail the update unless the condition is true. Usually used to limit
    the arguments that can be supplied to a contract choice.
    
    `assert (amount > 0)`
- `getTime <daml-ref-gettime>`  

    Gets the ledger effective time. Usually used to restrict when a
    choice can be exercised.
    
    `currentTime <- getTime`

- `return <daml-ref-return>`  

    Explicitly return a value. By default, a choice returns the result
    of its last update expression. This means you only need to use
    `return` if you want to return something else.
    
    `return ContractID ExampleTemplate`

The choice body can also contain:

- `let <daml-ref-let-update>` keyword  

    Used to assign values or functions.
- assign a value to the result of an update statement  

    For example: `contractFetched <- fetch someContractId`