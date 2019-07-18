---
linkTitle: "Data types"
title: "Reference: data types"
weight: 4
description: >
  This page gives reference information on DAML's data types.
---

## Built-in types

### Table of built-in primitive types

| Type      | For                                    | Example                           | Notes |
| --------- | -------------------------------------- | --------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Int`     | integers | `1`, `1000000`, `1_000_000`  | `Int` values are signed 64-bit integers which represent numbers between `-9,223,372,036,854,775,808` and `9,223,372,036,854,775,807` inclusive. Arithmetic operations raise an error on overflows and division by `0`. To make long numbers more readable you can optionally add underscores. |
| `Decimal` | fixed point decimals | `1.0`  | `Decimal` values are rational numbers with precision 38 and scale 10: numbers of the form `x / 10^10` where x is an integer with `\|x\| < 10^38`. |
| `Text`    | strings | `"hello"` | `Text` values are strings of characters enclosed by double quotes. |
| `Bool`    | boolean values  | `True`, `False`                   | |
| `Party`   | unicode string representing a party | `alice <- getParty "Alice"` | Every *party* in a DAML system has a unique identifier of type `Party`. To create a value of type `Party`, use binding on the result of calling `getParty`. The party text can only contain alphanumeric characters, `-`, `_` and spaces. |
| `Date`    | models dates | `date 2007 Apr 5` | To create a value of type `Date`, use the function `date` (to get this function, import `DA.Date`).|
| `Time`    | models absolute time (UTC) | `time (date 2007 Apr 5) 14 30 05` | `Time` values have microsecond precision. To create a value of type `Time`, use a `Date` and the function `time` (to get this function, import `DA.Time`). |
| `RelTime` | models differences between time values | `seconds 1`, `seconds (-2)` | `seconds 1` and `seconds (-2)` represent the values for 1 and -2 seconds. There are no literals for `RelTime`. Instead they are created using one of `days`, `hours`, `minutes` and `seconds` (to get these functions, import `DA.Time`). |

### Escaping characters

`Text` literals support backslash escapes to include their delimiter
(`\"`) and a backslash itself (`\\`).

### Time

Definition of time on the ledger is a property of the execution
environment. DAML assumes there is a shared understanding of what time
is among the stakeholders of contracts.

## Lists

`[a]` is the built-in data type for a list of elements of type `a`. The
empty list is denoted by `[]` and `[1, 3, 2]` is an example of a list of
type `[Int]`.

You can also construct lists using `[]` (the empty list) and `::` (which
is an operator that appends an element to the front of a list). For
example:

TODO rest of file

### Summing a list

## Records and record types

### Data constructors

### Accessing record fields

### Updating record fields

### Parameterized data types

## Type synonyms

### Function types

## Algebraic data types

### Product types

### Sum types

### Pattern matching

