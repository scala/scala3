---
layout: doc-page
title: "Implied Imports"
---

A special form of import is used to import implied instances. Example:
```scala
object A {
  class TC
  implied tc for TC
  def f given TC = ???
}
object B {
  import A._
  import implied A._
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the implied instance `tc`. Conversely, the second import `import implied A._` will import _only_ that implied instance.

Generally, a normal import clause brings all definitions except implied instances into scope whereas an `import implied` clause brings only implied instances into scope.

There are two main benefits arising from these rules:

 - It is made clearer where implied instances in scope are coming from. In particular, it is not possible to hide imported implied instances in a long list of regular imports.
 - It enables importing all implied instances
   without importing anything else. This is particularly important since implied
   instances can be anonymous, so the usual recourse of using named imports is not
   practical.

### Relationship with Old-Style Implicits

The rules of implied imports above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicits and
normal imports to implied instances and imports.

The following modifications avoid this hurdle to migration.

 1. An implied import also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a normal or
    by an implied import.

 2. In Scala 3.1, an old-style implicits accessed implicitly through a normal import
    will give a deprecation warning.

 3. In some version after 3.1, an old-style implicits accessed implicitly through a normal import
    will give a compiler error.

Thr rules mean that library users can use `import implied` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
implied instances once their user base has migrated.