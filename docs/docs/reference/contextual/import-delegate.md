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

### Importing By Type

Since delegates can be anonymous it is not always practical to import them by their name, and wildcard imports are typically used instead. By-type imports provide a more specific alternative to wildcard imports, which makes it clearer what is imported. Example:

```scala
import delegate A.{for TC}
```
This imports any delegate in `A` that has a type which conforms tp `TC`. There can be several bounding types following a `for` and bounding types can contain wildcards.
For instance, assuming the object
```scala
object Delegates {
  delegate intOrd for Ordering[Int]
  delegate [T: Ordering] listOrd for Ordering[List[T]]
  delegate ec for ExecutionContext = ...
  delegate im for Monoid[Int]
}
```
the import
```scala
import delegate Delegates.{for Ordering[_], ExecutionContext}
```
would import the `intOrd`, `listOrd`, and `ec` delegates but leave out the `im` delegate, since it fits none of the specified bounds.

By-type imports can be mixed with by-name imports. If both are present in an import clause, by-type imports come last. For instance, the import clause
```scala
import delegate Instances.{im, for Ordering[_]}
```
would import `im`, `intOrd`, and `listOrd` but leave out `ec`. By-type imports cannot be mixed with a wildcard import in the same import clause.

### Migration

The rules for delegate imports given above have the consequence that a library
would have to migrate in lockstep with all its users from old style implicits and
normal imports to delegate definitions and imports.

The following modifications avoid this hurdle to migration.

 1. A delegate import also brings old style implicits into scope. So, in Scala 3.0
    an old-style implicit definition can be brought into scope either by a normal import or by a delegate import.

 2. In Scala 3.1, old-style implicits accessed through a normal import
    will give a deprecation warning.

 3. In some version after 3.1, old-style implicits accessed through a normal import
    will give a compiler error.

These rules mean that library users can use `import delegate` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
representation clauses once their user base has migrated.
