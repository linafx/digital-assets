---
linkTitle: "Built-in functions"
title: "Reference: built-in functions"
weight: 5
description: >
  This page gives reference information on functions for working
  with time, numbers, text and lists.
---

## Working with time

DAML has these built-in functions for working with time:

- `datetime`: creates a `Time` given year, month, day, hours, minutes,
and seconds as argument.
- `subTime`: subtracts one time from another. Returns the `RelTime`
difference between `time1` and `time2`.
- `addRelTime`: add times. Takes a `Time` and `RelTime` and adds the
`RelTime` to the `Time`.
- `days`, `hours`, `minutes`, `seconds`: constructs a `RelTime` of the
specified length.
- `pass`: (in [scenario tests]({{< relref "scenarios.md" >}}) only) use `pass
: RelTime -> Scenario Time` to advance the ledger effective time by
the argument amount. Returns the new time.

## Working with numbers

DAML has these built-in functions for working with numbers:

- `round`: rounds a `Decimal` number to `Int`.
    
    `round d` is the *nearest* `Int` to `d`. Tie-breaks are resolved by
    rounding away from zero, for example:
    
    ``` none
    round 2.5 == 3    round (-2.5) == -3
    round 3.4 == 3    round (-3.7) == -4
    ```

- `truncate`: converts a `Decimal` number to `Int`, truncating the
    value towards zero, for example:
    
    ``` none
    truncate 2.2 == 2    truncate (-2.2) == -2
    truncate 4.9 == 4    v (-4.9) == -4
    ```

- `intToDecimal`: converts an `Int` to `Decimal`.

The set of numbers expressed by `Decimal` is not closed under division
as the result may require more than 10 decimal places to represent. For
example, `1.0 / 3.0 == 0.3333...` is a rational number, but not a
`Decimal`.

## Working with text

DAML has these built-in functions for working with text:

- `<>` operator: concatenates two `Text` values.
- `show` converts a value of the primitive types (`Bool`, `Int`,
`Decimal`, `Party`, `Time`, `RelTime`) to a `Text`.

To escape text in DAML strings, use `\`:

<table style="width:62%;">
<colgroup>
<col style="width: 25%" />
<col style="width: 37%" />
</colgroup>
<thead>
<tr class="header">
<th>Character</th>
<th>How to escape it</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>\</code></td>
<td><code>\\</code></td>
</tr>
<tr class="even">
<td><code>"</code></td>
<td><code>\"</code></td>
</tr>
<tr class="odd">
<td><code>'</code></td>
<td><code>\'</code></td>
</tr>
<tr class="even">
<td>Newline</td>
<td><code>\n</code></td>
</tr>
<tr class="odd">
<td>Tab</td>
<td><code>\t</code></td>
</tr>
<tr class="even">
<td>Carriage return</td>
<td><code>\r</code></td>
</tr>
<tr class="odd">
<td>Unicode (using <code>!</code> as an example)</td>
<td><ul>
<li>Decimal code: <code>\33</code></li>
<li>Octal code: <code>\o41</code></li>
<li>Hexadecimal code: <code>\x21</code></li>
</ul></td>
</tr>
</tbody>
</table>

## Working with lists

DAML has these built-in functions for working with lists:

  - `foldl` and `foldr`: see `daml-ref-folding` below.

### Folding

A *fold* takes:

- a binary operator
- a first *accumulator* value
- a list of values

The elements of the list are processed one-by-one (from the left in a
`foldl`, or from the right in a `foldr`).

<div class="note">

<div class="admonition-title">

Note

</div>

We'd usually recommend using `foldl`, as `foldr` is usually slower. This
is because it needs to traverse the whole list before starting to
discharge its elements.

</div>

Processing goes like this:

1.  The binary operator is applied to the first accumulator value and
    the first element in the list. This produces a second accumulator
    value.
2.  The binary operator is applied to the *second* accumulator value and
    the second element in the list. This produces a third accumulator
    value.
3.  This continues until there are no more elements in the list. Then,
    the last accumulator value is returned.

As an example, to sum up a list of integers in DAML:

TODO snippet