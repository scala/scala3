---
layout: doc-page
title: "Import Implicit"
---

A special form of import is used to import implicit instances. Example:
```scala
object A {
  class TC
  implicit tc for TC
  def f given TC = ???
}
object B {
  import A._
  import implicit A._
}
```
In the code above, the `import A._` clause of object `B` will import all members
of `A` _except_ the implicit `tc`. Conversely, the second import `import implicit A._` will import _only_ that implicit instance.

Generally, a normal import clause brings all members except implicit instances into scope whereas an `import implicit` clause brings only implicit instances into scope.

There are two main benefits arising from these rules:

 - It is made clearer where implicit instances in scope are coming from. In particular, it is not possible to hide imported implicit instances in a long list of regular imports.
 - It enables importing all implicit instances
   without importing anything else. This is particularly important since implicit
   instances can be anonymous, so the usual recourse of using named imports is not
   practical.

### Migration

The rules as stated above would break all existing code that imports implicits, which is of course unacceptable.
To make gradual migration possible, we adapt the following scheme.

 1. In Scala 3.0, a normal import will also import implicits written in the old "implicit-as-a-modifier" style.
    So these implicits can be brought into scope using either a normal import or an `import implicit`.

 2. In Scala 3.1, an old-style implicit accessed implicitly through a normal import will give a deprecation warning.

 3. In some version after 3.1, an old-style implicit accessed implicitly through a normal import
    will give a compiler error.

New-style implicit instance definitions always need to be imported with `import implicit`.

These rules mean that library users can use `import implicit` to access old-style implicits in Scala 3.0,
and will be gently nudged and then forced to do so in later versions. Libraries can then switch to
new-style implicit definitions once their user base has migrated.
