---
title: "6 Parties and authority"
weight: 7
---

# 6 Parties and authority

DAML is designed for distributed applications involving mutually
distrusting parties. In a well-constructed contract model, all parties
have strong guarantees that nobody cheats or circumvents the rules laid
out by templates and choices.

In this section you will learn about DAML's authorization rules and how
to develop contract models that give all parties the required
guarantees. In particular, you'll learn how to:

- Pass authority from one contract to another
- Write advanced choices
- Reason through DAML's Authorization model

## Preventing IOU revocation

The `SimpleIou` contract from
[4 Transforming data using choices]({{< relref "4-transformations.md" >}}) and
[5 Adding constraints to a contract]({{< relref "5-restrictions.md" >}})
has one major problem: The contract is only signed by the `issuer`. The
signatories are the parties with the power to create and archive
contracts. If Alice gave Bob a `SimpleIou` for $100 in exchange for some
goods, she could just archive it again after receiving the goods. Bob
would have a record such actions, but would have to resort to off-ledger
means to get his money back.

TODO rest of file.

## Use propose-accept workflows for one-off authorization

## Use role contracts for ongoing authorization

## DAML's authorization model

### An authorization example

## Next up

In [7 Composing choices]({{< relref "7-composing.md" >}}) you will finally
put everything you have learned
together to build a simple asset holding and trading model akin to that
in the [Quickstart]({{< relref "quickstart.md" >}}). In that context you'll
learn a bit more about the `Update` action and how to use it to compose
transactions, as well as about privacy on DAML ledgers.

