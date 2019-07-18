---
title: "3 Data types"
weight: 4
---

# 3 Data types

In [Basic contracts]({{< relref "1-token.md" >}}), you learnt about contract templates, which specify the
types of contracts that can be created on the ledger, and what data
those contracts hold in their arguments.

In [Testing templates using scenarios]({{< relref "2-scenario.md" >}}), you learnt about the scenario view in DAML Studio,
which displays the current ledger state. It shows one table per
template, with one row per contract of that type and one column per
field in the arguments.

This actually provides a useful way of thinking about templates: like
tables in databases. Templates specify a data schema for the ledger:

- each template corresponds to a table
- each field in the `with` block of a template corresponds to a column
in that table
- each contract instance of that type corresponds to a table row

In this section, you'll learn how to create rich data schemas for your
ledger. Specifically you'll learn about:

- DAML's built-in and native data types
- Record types
- Derivation of standard properties
- Variants
- Manipulating immutable data
- Contract keys

After this section, you should be able to use a DAML ledger as a simple
database where individual parties can write, read and delete complex
data.

## Native types

You have already encountered a few native DAML types: `Party` in
[Basic contracts]({{< relref "1-token.md" >}}), and `Text` and `ContractId` in
[Testing templates using scenarios]({{< relref "2-scenario.md" >}}). Here are those
native types and more:

- `Party`

  Stores the identity of an entity that is able to act on the
  ledger, in the sense that they can sign contracts and submit
  transactions. In general, `Party` is Oblique.
- `Text`

  Stores a unicode character string like `"Alice"`.
- `ContractId a`

  Stores a reference to a contract of type `a`.
- `Int`

  Stores signed 64-bit integers. For example, `-123`.
- `Decimal`

  Stores fixed-point number with 28 digits before and 10
digits after the decimal point. For example, `0.0000000001` or
`-9999999999999999999999999999.9999999999`.
- `Bool`

  Stores `True` or `False`.
- `Date`

  Stores a date.
- `Time`

  Stores absolute UTC time.
- `RelTime`

  Stores a difference in time.

The below scenario instantiates each one of these types, manipulates it
where appropriate, and tests the result.

TODO rest of file.

## Assembling types

### Tuples

### Lists

### Records

### Variants and pattern matching

## Manipulating data

## Contract keys

## Next up

You can now define data schemas for the ledger, read, write and delete data from the ledger, and use keys to reference and look up data in a stable fashion.

In [4 Transforming data using choices]({{< relref "4-transformations.md" >}}) you'll learn how to define data transformations and give other parties the right to manipulate data in restricted ways.