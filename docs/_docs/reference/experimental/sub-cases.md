---
layout: doc-page
title: "Match Expressions with Sub Cases"
---

A case in a match expression can be followed by another sub-match expression, introduced by the `if` keyword.
For example:

```scala
enum Version:
  case Legacy
  case Stable(major: Int, minor: Int)

case class Document(title: String, version: Version)

def version(d: Option[Document]) = d match
  case Some(x) if x.version match
    case Version.Stable(m, n) if m > 2 => s"$m.$n"
    case Version.Legacy => "legacy"
  case _ => "unsupported"

assert(version(Some(Document("...", Version.Stable(3, 1)))) = "3.1")
assert(version(Some(Document("...", Version.Stable(2, 1)))) = "unsupported")
assert(version(Some(Document("...", Version.Legacy))) = "legacy")
assert(version(Some(Document("...", None))) = "unsupported")
```

The cases of a sub-match expression are tested iff the outer case matches.
The sub match scrutinee can refer to variables bound from the outer pattern.
Evaluation of sub-matches then proceeds as usual. 
For example, if `version` is applied on `Some(Document("...", Version.Stable(3, 1)))`, the first outer pattern matches (i.e., `Some(x)`), causing the sub match expression to be evaluated with scrutinee `Version.Stable(3, 1)`, yielding `"3.1"`.

The cases of a sub-match expression need not be exhaustive.
If they were, we would not need sub-match at all: a usual match in the body of the first case would suffice,
e.g., `case Some(x) => x.version match ...`.
If none of the sub-cases succeed, then control flow returns to the outer match expression and proceeds as though the current case had not matched.
For example, `Some(Document("...", Version.Stable(2, 1)))` matches the first pattern, but none of its sub-cases, and we therefore obtain the result `"unsupported"`. 


## Motivation

Without sub matches, one would typically duplicate either the default case or the outer pattern.
That is, use:
```scala
def version(d: Option[Document]) = d match
  case Some(x) => x.version match
    case Version.Stable(m, n) if m > 2 => s"$m.$n"
    case Version.Legacy => "legacy"
    case _ => "unsupported"
  case _ => "unsupported"
```
or
```scala
def version(d: Option[Document]) = d match
  case Some(Document(_, Version.Stable(m, n))) if m > 2 => s"$m.$n"
  case Some(Document(_, Version.Legacy)) => "legacy"
  case _ => "unsupported"
```

## Details

Sub-cases allow:
- Arbitrary nesting, e.g. sub-sub-matches are supported.
- Interleaving boolean guards, e.g. `case Some(x: Int) if x != 0 if x match ...`.
- Interleaving pattern extractors and computations for the scrutinees of sub-matches.

Sub-cases are supported for:
- match clauses
- catch clauses
- partial functions

Similarly to catch clauses, match expressions with a single case can now be written on single line (without braces),
e.g., `Some(1) match case Some(x) => x`.

Exhaustivity and reachability checking conservatively assume the sub-cases to be partial, similarly boolean guards.

A sub-match is inlined iff the outer match is inlined, with the same semantics as the usual match expressions.
