---
layout: doc-page
title: "Dropped: Class Shadowing - More Details"
movedTo: https://docs.scala-lang.org/scala3/reference/dropped-features/class-shadowing-spec.html
---

Spec diff: in section [5.1.4 Overriding](https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html#Overriding), add *M' must not be a class*.

> Why do we want to make this change to the language?

Class shadowing is irregular compared to other types of overrides. Indeed, inner classes are not actually overridden but simply shadowed.


> How much existing code is going to be affected?

From all the code compiled so far with Scala 3 the only instance of this I could find is in the stdlib. Looking at [this commit](https://github.com/lampepfl/scala/commit/68f13bf39979b631ed211ec1751934306ceb5d6c#diff-7aa508b70e055b47c823764e3e5646b8) it seems like the usage of class shadowing was accidental.


> How exactly is existing code going to be affected?

Code that relies on overridden inner classes will stop compiling.


> Is this change going to be migratable automatically?

No.
