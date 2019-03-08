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
