---
layout: doc-page
title: "Imports of Representatives"
---

A special form of import is used to import representatives. Example:
```scala
object A {
  class TC
  repr tc of TC
  def f given TC = ???
}
object B {
  import A._
  import repr A._
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the representative `tc`. Conversely, the second import `import repr A._` will import _only_ that representative.

Generally, a normal import clause brings all members except representatives into scope whereas an `import repr` clause brings only representatives into scope.

There are two main benefits arising from these rules:

 - It is made clearer where representatives in scope are coming from.
   In particular, it is not possible to hide imported representatives in a long list of regular imports.
 - It enables importing all representatives
   without importing anything else. This is particularly important since representatives
   can be anonymous, so the usual recourse of using named imports is not
   practical.

### Migration

The rules of representatives above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicits and
normal imports to representatives and `import repr` clauses.

The following modifications avoid this hurdle to migration.

 1. An `import repr` also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a normal import or
    by an `import repr`.

 2. In Scala 3.1, old-style implicits accessed through a normal import
    will give a deprecation warning.

 3. In some version after 3.1, old-style implicits accessed through a normal import
    will give a compiler error.

These rules mean that library users can use `import repr` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
`repr` clauses once their user base has migrated.
