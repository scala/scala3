---
layout: doc-page
title: "Motivation"
---

### Critique

Scala's implicits are its most distinguished feature. They are _the_ fundamental way to abstract over context. They represent a single concept with an extremely varied number of use cases, among them: implementing type classes, establishing context, dependency injection, expressing capabilities, computing new types and proving relationships between them.

At the same time, implicits are also a controversial feature. I believe there are several reasons for this.

First, being very powerful, implicits are easily over-used and mis-used. This observation holds in almost all cases when we talk about _implicit conversions_, which, even though conceptually different, share the same syntax with other implicit definitions. For instance,
regarding the two definitions

    implicit def i1(implicit x: T): C[T] = ...
    implicit def i2(x: T): C[T] = ...

the first of these is a conditional implicit _value_, the second an implicit _conversion_. Conditional implicit values are a cornerstone for expressing type classes, whereas most applications of implicit conversions have turned out to be of dubious value. The problem is that many newcomers to the language start with defining implicit conversions since they are easy to understand and seem powerful and convenient. Scala 3 will put under a language flag both definitions and applications of "undisciplined" implicit conversions between types defined elsewhere. This is a useful step to push back against overuse of implicit conversions. But the problem remains that syntactically, conversions and values just look too similar for comfort.

Second, implicits pose challenges for tooling. The set of available implicits depends on context, so command completion has to take context into account. This is feasible in an IDE but docs like ScalaDoc that are based static web pages can only provide an approximation. Another problem is that failed implicit searches often give very unspecific error messages, in particular if some deeply recursive implicit search has failed. The dotty compiler implements some improvements in this case, but further progress would be desirable.

Third, the syntax of implicit definitions might be a bit too minimal. It consists of a single modifier, `implicit`, that can be attached to a large number of language constructs. A problem with this for newcomers is that it often conveys mechanism better than intent. For instance, a typeclass instance is an implicit object or val if unconditional and an implicit def with implicit parameters if conditional. This describes precisely what the implicit definitions translate to -- just drop the `implicit` modifier, and that's it! But the cues that define intent are rather indirect and can be easily misread, as demonstrated by the definitions of `i1` and `i2` above.

Fourth, the syntax of implicit parameters has also some shortcomings. It starts with the position of `implicit` as a pseudo-modifier that applies to a whole parameter section instead of a single parameter. This represents an irregular case wrt to the rest of Scala's syntax. Furthermore, while implicit _parameters_ are designated specifically, arguments are not. Passing an argument to an implicit parameter looks like a regular application `f(arg)`. This is problematic because it means there can be confusion regarding what parameter gets instantiated in a call. For instance, in
```scala
def currentMap(implicit ctx: Context): Map[String, Int]
```
one cannot write `currentMap("abc")` since the string "abc" is taken as explicit argument to the implicit `ctx` parameter. One has to write `currentMap.apply("abc")` instead, which is awkward and irregular. For the same reason, a method definition can only have one implicit parameter section and it must always come last. This restriction not only reduces orthogonality, but also prevents some useful program constructs, such as a method with a regular parameter whose type depends on an implicit value. Finally, it's also a bit annoying that implicit parameters must have a name, even though in many cases that name is never referenced.

None of the shortcomings is fatal, after all implicits are very widely used, and many libraries and applications rely on them. But together, they make code using implicits more cumbersome and less clear than it could be.

Can implicit function types help? Implicit function types allow to abstract over implicit parameterization. They are a key part of the program to make as many aspects of methods as possible first class. Implicit function types can avoid much of the repetition in programs that use implicits widely. But they do not directly address the issues mentioned here.

### Alternative Design

`implicit` is a modifier that gets attached to various constructs. I.e. we talk about implicit vals, defs, objects, parameters, or arguments. This conveys mechanism rather than intent. What _is_ the intent that we want to convey? Ultimately it's "trade types for terms". The programmer specifies a type and the compiler fills in the term matching that type automatically. So the concept we are after would serve to express definitions that provide the canonical instances for certain types.

I believe a good name for this concept is _witness_. A term is a witness for a type if it is an implicit instance of this type. It is secondary whether this instance takes the form of a `val` or `object` or whether it is a method. It would be better to have a uniform syntax for all of these kinds of instances.

The next sections elaborate such an alternative design. It consists of two proposals which are independent of each other:

 - A proposal to replace implicit _definitions_ by [witness definitions](./witnesses.html).
 - A proposal for a [new syntax](./witness-params.html) of implicit _parameters_ and their _arguments_.
