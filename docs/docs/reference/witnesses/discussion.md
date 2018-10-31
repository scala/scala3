---
layout: doc-page
title: "Discussion"
---

## Summary

The witness proposal consists of two main parts:

 - Define a new [high-level syntax for witnesses](./witnesses.html) that works out better the intent underlying implicit definitions.
 - Define a [new syntax for implicit parameters](./witness-params.html) that aligns formal parameters and arguments.
 - Define [abstract and alias witnesses](./replacing-implicits.html) and replace all existing usages of `implicit` in the language.


## Migration

New and old syntax would co-exist initially. Rewrite rules could rewrite old syntax to new automatically. This is trivial in the case of implicit parameters and implicit function types. It is a bit more involved in the case of implicit definitions, since more extensive pattern matching is required to recognize a definition that can be rewritten to a witness.

The third part (replacing existing implicits) should be adopted well after the first two parts
are implemented. Alias and abstract witnesses could be introduced together with the other witness definitions, but could also come later.

## Discussion

This is a rather sweeping proposal, which will affect most Scala code. Here are some supporting arguments and a summary of alternatives that were considered.

The witness definition syntax makes the definition of implicit instances clearer and more concise. People have repeatedly asked for specific "typeclass syntax" in Scala. I believe that witnesses together with extension methods address this ask quite well.

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

A contentious point is whether we want abstract and alias witnesses. As an alternative, one would could also keep the current syntax for implicit vals and defs, which can express the same concepts. The main advantage to introduce abstract and alias witnesses is that it would
allow us to drop implicit definitions altogether.