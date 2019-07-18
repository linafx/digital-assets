---
linkTitle: "Expressions"
title: "Reference: expressions"
weight: 6
description: >
  This page gives reference information on DAML expressions that
  are not Updates.
---

## Definitions

### Values

### Functions

## Arithmetic operators

| Operator                          | Works for                   |
| --------------------------------- | --------------------------- |
| `+`                               | `Int`, `Decimal`, `RelTime` |
| `-`                               | `Int`, `Decimal`, `RelTime` |
| `*`                               | `Int`, `Decimal`            |
| `/` (integer division)            | `Int`                       |
| `%` (integer remainder operation) | `Int`                       |
| `^` (integer exponentiation)      | `Int`                       |

The result of the modulo operation has the same sign as the dividend:

- `7 / 3` and `(-7) / (-3)` evaluate to `2`
- `(-7) / 3` and `7 / (-3)` evaluate to `-2`
- `7 % 3` and `7 % (-3)` evaluate to `1`
- `(-7) % 3` and `(-7) % (-3)` evaluate to `-1`

To write infix expressions in prefix form, wrap the operators in
parentheses. For example, `(+) 1 2` is another way of writing `1 + 2`.

## Comparison operators

| Operator             | Works for                                                                                                                                                           |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<`, `<=`, `>`, `>=` | `Bool`, `Text`, `Int`, `Decimal`, `Party`, `Time`                                                                                                                   |
| `==`, `/=`           | `Bool`, `Text`, `Int`, `Decimal`, `Party`, `Time`, and `identifiers of contract instances <daml-ref_contract-identifiers>` stemming from the same contract template |

## Logical operators

The logical operators in DAML are:

- `not` for negation, e.g., `not True == False`
- `&&` for conjunction, where `a && b == and a b`
- `||` for disjunction, where `a || b == or a b`

for `Bool` variables `a` and `b`.

## If-then-else

## let