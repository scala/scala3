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

To make gradual change possible, we allow the new `with` application syntax also for
old style implicit parameters.

One tricky question concerns context bounds. During the migration period, should they map to old style implicit parameters or new style context parameters? Mapping them to context parameters could break things in difficult to diagnose ways since then an explicit argument for a context bound would be treated as a type error, or, in the worst case, would be constructed as an argument for an `apply` of the result of method with the
context bound. Also, it would remove context bounds from the common subset that is
treated the same in Scala 2 and 3. We therefore opt to map context bounds to old style implicit parameters for the time being. In the future, migrating context bounds implies a three-step process:

 - Step 1: Deprecate passing arguments to evidence parameters defined by context bounds
   directly. All such parameters should be passed with `with`.
 - Step 2: Remove ability to pass context bound arguments directly.
 - Step 3: Map context bounds to context parameters.

The third part (replacing existing implicits) should be adopted well after the first two parts are implemented. Alias and abstract witnesses could be introduced together with the other witness definitions, but could also come later.

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