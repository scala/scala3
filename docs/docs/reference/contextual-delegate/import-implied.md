---
layout: doc-page
title: "Delegate Imports"
---

A special form of import is used to import delegates. Example:
```scala
object A {
  class TC
  delegate tc for TC
  def f given TC = ???
}
object B {
  import A._
  import delegate A._
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the delegate `tc`. Conversely, the second import `import delegate A._` will import _only_ that delegate.

Generally, a normal import clause brings all definitions except delegates into scope whereas a delegate import brings only delegates into scope.

There are two main benefits arising from these rules:

 - It is made clearer where delegates in scope are coming from.
   In particular, it is not possible to hide imported delegates in a long list of regular imports.
 - It enables importing all delegates
   without importing anything else. This is particularly important since delegates
   can be anonymous, so the usual recourse of using named imports is not
   practical.

### Relationship with Old-Style Implicits

The rules of delegates above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicits and
normal imports to delegate clauses and delegate imports.

The following modifications avoid this hurdle to migration.

 1. An delegate import also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a normal import or by an `import delegate`.

 2. In Scala 3.1, old-style implicits accessed through a normal import
    will give a deprecation warning.

 3. In some version after 3.1, old-style implicits accessed through a normal import
    will give a compiler error.

These rules mean that library users can use `import delegate` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
representation clauses once their user base has migrated.
