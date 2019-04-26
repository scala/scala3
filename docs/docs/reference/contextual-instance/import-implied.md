---
layout: doc-page
title: "Instance Imports"
---

A special form of import is used to import implicit instances. Example:
```scala
object A {
  class TC
  instance tc of TC
  def f given TC = ???
}
object B {
  import A._
  import instance A._
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the instance `tc`. Conversely, the second import `import instance A._` will import _only_ that instance.

Generally, a normal import clause brings all members except implicit instance values into scope whereas an `import instance` clause brings only implicit instance values into scope.

There are two main benefits arising from these rules:

 - It is made clearer where instance values in scope are coming from.
   In particular, it is not possible to hide imported instance values
   in a long list of regular imports.
 - It enables importing all instance values
   without importing anything else. This is particularly important since implicit
   instances can be anonymous, so the usual recourse of using named imports is not
   practical.

### Relationship with Old-Style Implicits

The rules of evidence imports above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicit definitions and
normal imports to evidence definitions and evidence imports.

The following modifications avoid this hurdle to migration.

 1. An evidence import also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a normal or
    by an evidence import.

 2. In Scala 3.1, an old-style implicits accessed implicitly through a normal import
    will give a deprecation warning.

 3. In some version after 3.1, an old-style implicits accessed implicitly through a normal import
    will give a compiler error.

These rules mean that library users can use `import evidence` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
evidence definitions once their user base has migrated.
