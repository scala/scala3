---
layout: doc-page
title: "Discussion"
---

## Summary

The witness proposal consists of two main parts:

 - Define a new [high-level syntax for witnesses](./witnesses.html) that works out better the intent underlying implicit definitions.
 - Define a [new syntax for implicit parameters](./witness-params.html) that aligns formal parameters and arguments.

## Other Uses of `implicit`

The only use cases that are not yet covered by the proposal are implicit conversions and implicit classes. We do not propose to use `witness` in place of `implicit` for these, since that would bring back the uncomfortable similarity between implicit conversions and parameterized implicit aliases. However, there is a way to drop implicit conversions entirely. Scala 3 already [defines](https://github.com/lampepfl/dotty/pull/2065) a class `ImplicitConverter` whose instances are available as implicit conversions.
```scala
  abstract class ImplicitConverter[-T, +U] extends Function1[T, U]
```
One could define all implicit conversions as witnesses of this class. E.g.
```scala
witness StringToToken for ImplicitConverter[String, Token] {
  def apply(str: String): Token = new KeyWord(str)
}
```
The fact that this syntax is more verbose than simple implicit defs could be a welcome side effect since it might dampen any over-enthusiasm for defining implicit conversions.

That leaves implicit classes. Most use cases of implicit classes are probably already covered by extension methods. For the others, one could always fall back to a pair of a regular class and an `ImplicitConverter` witness. It would be good to do a survey to find out how many classes would be affected.

## Migration

New and old syntax would co-exist initially. Rewrite rules could rewrite old syntax to new automatically. This is trivial in the case of implicit parameters and implicit function types. It is a bit more involved in the case of implicit definitions, since more extensive pattern matching is required to recognize a definition that can be rewritten to a witness.

## Discussion

This is a rather sweeping proposal, which will affect most Scala code. Here are some supporting arguments and a summary of alternatives that were considered.

The witness definition syntax makes the definition of implicit instances clearer and more concise. People have repeatedly asked for specific "typeclass syntax" in Scala. I believe that witnesses together with extension methods address this ask quite well.

A contentious point is whether we want abstract and alias witnesses. As an alternative, would could also keep the current syntax for implicit vals and defs, which can express the same concepts. The main advantage to introduce abstract and alias witnesses is that it would
allow us to drop implicit definitions altogether.

Probably the most far reaching and contentious changes affect implicit parameters. There might be resistance to change, because the existing scheme seems to work "well enough". However, I believe there is a price to pay for the status quo. The need to write `.apply` occasionally to force implicit arguments is already bad. Worse is the fact that implicits always have to come last, which makes useful program patterns much more cumbersome than before and makes the language less regular.

Several alternatives to the proposed syntax changes for implicit parameters were considered:

 1. Leave `implicit` parameters as they are. This suffers from the problems stated
   in the [motivation section](./motivation.md).
 2. Leave the syntax of `implicit` parameters but institute two changes: First, applications
   of implicit parameters must be via the pseudo method `.explicitly(...)`. Second, there can be more than one implicit parameter list and implicit parameters may precede explicit ones. This fixes most of the discussed problems, but at the expense of a bulky explicit application syntax. Bulk can be a problem, for instance when the programmer tries to
   construct an extensive explicit argument tree to figure out what went wrong with a missing
   implicit. Another issue is that migration from old to new scheme would be tricky and
   would likely take multiple language versions.
 3. Split the meanings of implicitly passed parameters and witness parameters. Use prefix ‘.’     as a syntax to indicate that an argument for a parameter can be passed explicitly. Use
   `witness` as a parameter modifier to indicate that the parameter is available as a witness.
   This scheme admits some new patterns, such as an explicit parameter that can be
   used as a witness, or an implicitly passed parameter that is not a witness itself.
   But the syntax looks unfamiliar and suffers from the choice paradox.
