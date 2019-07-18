---
title: "4 Transforming data using choices"
weight: 5
---

# 4 Transforming data using choices

In the example in [Contract keys]({{< relref "3-data.md#contract-keys" >}})
the accountant party wanted to change
some data on a contract. They did so by archiving the contract and
re-creating it with the updated data. That works because the accountant
is the sole signatory on the `Account` contract defined there.

But what if the accountant wanted to allow the bank to change their own
telephone number? Or what if the owner of a `CashBalance` should be able
to transfer ownership to someone else?

In this section you will learn about how to define simple data
transformations using *choices* and how to delegate the right to
*exercise* these choices to other parties.

## Choices as methods

If you think of templates as classes and contracts as objects, where are
the methods?

Take as an example a `Contact` contract on which the contact owner wants
to be able to change the telephone number, just like on the `Account` in
[Contract keys]({{< relref "3-data.md#contract-keys" >}}). Rather than requiring
them to manually look up the
contract, archive the old one and create a new one, you can provide them
a convenience method on `Contact`:

TODO rest of file.

## Choices as delegation

## Choices in the Ledger Model

### The Archive choice

## A simple cash model

## Next up

You can now store and transform data on the ledger, even giving other parties specific write access through choices.

In [5 Adding constraints to a contract]({{< relref "5-restrictions.md" >}}), you will learn how to restrict data and transformations further. In that context, you will also learn about time on DAML ledgers, `do` blocks and `<-` notation within those.