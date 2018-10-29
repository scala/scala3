---
layout: doc-page
title: "Discussion"
---

## Summary

The witness proposal consists of three main parts:

 - Define a new [high-level syntax for witnesses](./witnesses.html) that works out better the intent underlying implicit definitions.
 - Split the two meanings of implicit parameters into [separate concepts](./witness-params.html). One concept specifies that a parameter is implicitly applied, the other makes the parameter available as a witness.
  - Allow `witness` as a [modifier](./witness-modifier.html) to replace remaining use cases for implicit definitions and to provide a lower level syntax into which witness definitions (with `witness` as a subject) can be translated.

## Other Uses of `implicit`

The only use cases that are not yet covered by the proposal are implicit classes and implicit conversions. We do not propose to use `witness` in place of `implicit` for these, since that would bring back the uncomfortable similarity between implicit conversions and parameterized implicit aliases. However, there is a way to drop implicit conversions entirely. Scala 3 already [defines](https://github.com/lampepfl/dotty/pull/2065) a class `ImplicitConverter` whose instances are available as implicit conversions.
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

Probably the most far reaching and contentious changes affect implicit parameters. There might be resistance to change, because the existing scheme seems to work "well enough". However, I believe there is a price to pay for the status quo. The need to write `.apply` occasionally to force implicit arguments is already bad. Worse is the fact that implicits always have to come last, which makes useful program patterns much more cumbersome than before and makes the language less regular. Also problematic is the
lack of scope control for implicit parameters which caused the invention of macro-based libraries such as MacWire for what looks like an ideal task for implicits. Without the
changes proposed here, dependency injection in the style of MacWire will no longer be possible in Scala 3, since whitebox macros are going away.

Several alternatives to the proposed syntax changes for implicit parameters were considered:

 1. Leave `implicit` parameters as they are. This suffers from the problems stated
   in the [motivation section](./motivation.md).
 2. Leave the syntax of `implicit` parameters but institute two changes: First, applications
   of implicit parameters must be via the pseudo method `.explicitly(...)`. Second, there can be more than one implicit parameter list and implicit parameters may precede explicit ones. This fixes most of the discussed problems, but at the expense of a bulky explicit application syntax. Bulk can be a problem, for instance when the programmer tries to
   construct an extensive explicit argument tree to figure out what went wrong with a missing
   implicit. Another issue is that migration from old to new scheme would be tricky and
   would likely take multiple language versions.
 3. The design presented here, but with `implicit` instead of `witness` as the modifier for
   parameters. This is closer to the status quo, but migration presents problems: At what point will an `implicit` modifier stop having the old meaning and acquire the new one? Using `witness` instead of `implicit` solves that problem because old and new styles can coexist and it is always clear which is which.
 4. Don't split the meanings of implicitly passed parameters and witness parameters. Use prefix ‘.’ as a syntax
   for both meanings together. This is more concise and relieves the programmer from having to choose
   which combination of functionality is desired. On the other hand, this does not support some
   use patterns such as MacWire style dependency injection. Also, the prefix ‘.’ syntax is maybe
   a bit too inconspicuous at the parameter definition site, even though it works very well at the
   function application site.
 5. As in 4., but using `witness` or `implicit` as the syntax that marks an implicitly passed witness parameter.
   This breaks the correspondence between function abstraction and application syntax.

Once we have high-level witness definitions and witness parameters it's a small step to convert most the remaining uses of `implicit` to `witness` as a modifier. There are no stringent technical reasons for doing so, but it looks more consistent with the other changes.
