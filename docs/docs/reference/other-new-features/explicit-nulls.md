---
layout: doc-page
title: "Explicit Nulls"
---

This proposal describes a modification to the Scala type system that makes reference types
(anything that extends `AnyRef`) _non-nullable_.

This means the following code will no longer typecheck:
```
val x: String = null // error: found `Null`,  but required `String`
```

Instead, to mark a type as nullable we use a [type union](https://dotty.epfl.ch/docs/reference/new-types/union-types.html)
```
val x: String|Null = null // ok
```

Explicit nulls are enabled via a `-Yexplicit-nulls` flag, so they're an opt-in feature.

Read on for details.

## New Type Hierarchy

When explicit nulls are enabled, the type hierarchy changes so that `Null` is subtype only of
`Any`, as opposed to every reference type.

This is the new type hierarchy:
![](../../images/explicit-nulls/explicit-nulls-type-hierarchy.png "Type Hierarchy for Explicit Nulls")

After erasure, `Null` remains a subtype of all reference types (as forced by the JVM).

## Unsoundness

The new type system is unsound with respect to `null`. This means there are still instances where an expressions has a non-nullable type like `String`, but its value is `null`.

The unsoundness happens because uninitialized fields in a class start out as `null`:
```scala
class C {
  val f: String = foo(f)
  def foo(f2: String): String = if (f2 == null) "field is null" else f2 
}
val c = new C()
// c.f == "field is null"
```

Enforcing sound initialization is a non-goal of this proposal. However, once we have a type
system where nullability is explicit, we can use a sound initialization scheme like the one
proposed by @liufengyun and @biboudis in [https://github.com/lampepfl/dotty/pull/4543](https://github.com/lampepfl/dotty/pull/4543) to eliminate this particular source of unsoundness. 

## Equality

Because of the unsoundness, we need to allow comparisons of the form `x == null` or `x != null`
even when `x` has a non-nullable reference type (but not a value type). This is so we have an
"escape hatch" for when we know `x` is nullable even when the type says it shouldn't be.
```scala
val x: String|Null = null
x == null       // ok: x is a nullable string
"hello" == null // ok: String is a reference type
1 == null       // error: Int is a value type
```

### Reference Equality

Recall that `Null` is now a direct subtype of `Any`, as opposed to `AnyRef`.
However, we also need to allow reference equality comparisons:
```scala
val x: String = null
x eq null // ok: could return `true` because of unsoundness
```

We support this case by making the `eq` and `ne` methods in `AnyRef` take a `AnyRef|Null`
as argument.

We also need to support
```scala
null.eq("hello")
val x: String|Null = null
x.eq(null)
```

We support this case via extension methods defined in the Predef:
```scala
def (x: AnyRef|Null) eq(y: AnyRef|Null): Boolean =
  (x == null && y == null) || (x != null && x.eq(y))
```

## Working with Null

To make working with nullable values easier, we propose adding a few utilities to the standard library.
So far, we have found the following useful:
  - An extension method `.nn` to "cast away" nullability
    ```scala
    implicit class NonNull[T](x: T|Null) extends AnyVal {
      def nn: T = if (x == null) {
        throw new NullPointerException("tried to cast away nullability, but value is null")
      } else {
        x.asInstanceOf[T]
      }
    }
    ```
    This means that given `x: String|Null`, `x.nn` has type `String`, so we can call all the
    usual methods on it. Of course, `x.nn` will throw a NPE if `x` is `null`.

## Java Interop

The compiler can load Java classes in two ways: from source or from bytecode. In either case,
when a Java class is loaded, we "patch" the type of its members to reflect that Java types
remain implicitly nullable.

Specifically, we patch
  * the type of fields
  * the argument type and return type of methods

### Nullification Function

We do the patching with a "nullification" function `n` on types:
```scala
1. n(T)              = T|JavaNull              if T is a reference type
2. n(T)              = T                       if T is a value type
3. n(T)              = T|JavaNull              if T is a type parameter
4. n(C[T])           = C[T]|JavaNull           if C is Java-defined
5. n(C[T])           = C[n(T)]|JavaNull        if C is Scala-defined
6. n(A|B)            = n(A)|n(B)|JavaNull
7. n(A&B)            = (n(A)&n(B))|JavaNull
8. n((A1, ..., Am)R) = (n(A1), ..., n(Am))n(R) for a method with arguments (A1, ..., Am) and return type R
9. n(T)              = T                       otherwise
```

`JavaNull` is an alias for `Null` with magic properties (see below). We illustrate the rules for `nf` below with examples.

  * The first two rules are easy: we nullify reference types but not value types.
    ```scala
    class C {
      String s;
      int x;
    }
    ==>
    class C {
      val s: String|Null
      val x: Int
    }
    ```

  * In rule 3 we nullify type parameters because in Java a type parameter is always nullable, so the following code compiles.
    ```scala
    class C<T> { T foo() { return null; } }
    ==>
    class C[T] { def foo(): T|Null } 
    ```

    Notice this is rule is sometimes too conservative, as witnessed by
    ```scala
    class InScala {
      val c: C[Bool] = ???  // C as above
      val b: Bool = c.foo() // no longer typechecks, since foo now returns Bool|Null
    }
    ```
    
  * Rule 4 reduces the number of redundant nullable types we need to add. Consider
    ```scala
    class Box<T> { T get(); }
    class BoxFactory<T> { Box<T> makeBox(); }
    ==>
    class Box[T] { def get(): T|JavaNull }
    class BoxFactory[T] { def makeBox(): Box[T]|JavaNull }
    ```
    
    Suppose we have a `BoxFactory[String]`. Notice that calling `makeBox()` on it returns a `Box[String]|JavaNull`, not
    a `Box[String|JavaNull]|JavaNull`, because of rule 4. This seems at first glance unsound ("What if the box itself
    has `null` inside?"), but is sound because calling `get()` on a `Box[String]` returns a `String|JavaNull`, as per
    rule 3.
    
    Notice that for rule 4 to be correct we need to patch _all_ Java-defined classes that transitively appear in the
    argument or return type of a field or method accessible from the Scala code being compiled. Absent crazy reflection
    magic, we think that all such Java classes _must_ be visible to the Typer in the first place, so they will be patched.
        
  * Rule 5 is needed because Java code might use a generic that's defined in Scala as opposed to Java.
    ```scala
    class BoxFactory<T> { Box<T> makeBox(); } // Box is Scala defined
    ==>
    class BoxFactory[T] { def makeBox(): Box[T|JavaNull]|JavaNull }
    ```
    
    In this case, since `Box` is Scala-defined, `nf` is applied to the type argument `T`, so rule 3 applies and we get
    `Box[T|JavaNull]|JavaNull`. This is needed because our nullability function is only applied (modularly) to the Java
    classes, but not to the Scala ones, so we need a way to tell `Box` that it contains a nullable value.

  * Rules 6, 7, and 8 just recurse structurally on the components of the type.
    The handling of unions and intersections in the compiler is a bit more involved than the presentation above.
    Specifically, the implementation makes sure to add `| Null` only at the top level of a type:
    e.g. `nf(A & B) = (A & B) | JavaNull`, as opposed to `(A | JavaNull) & (B | JavaNull)`.
  
### JavaNull
To enable method chaining on Java-returned values, we have a special `JavaNull` alias
```scala
type JavaNull = Null
```

`JavaNull` behaves just like `Null`, except it allows (unsound) member selections:
```scala
// Assume someJavaMethod()'s original Java signature is
// String someJavaMethod() {}
val s2: String = someJavaMethod().trim().substring(2).toLowerCase() // unsound
```

Here, all of `trim`, `substring` and `toLowerCase` return a `String|JavaNull`.
The Typer notices the `JavaNull` and allows the member selection to go through.
However, if `someJavaMethod` were to return `null`, then the first member selection
would throw a `NPE`.

Without `JavaNull`, the chaining becomes too cumbersome
```scala
val ret = someJavaMethod()
val s2 = if (ret != null) {
  val tmp = ret.trim()
  if (tmp != null) {
    val tmp2 = tmp.substring(2)
    if (tmp2 != null) {
      tmp2.toLowerCase()
    }
  }
}
// Additionally, we need to handle the `else` branches.
```

## Binary Compatibility

Our strategy for binary compatibility with Scala binaries that predate explicit nulls
is to leave the types unchanged and be compatible but unsound.

Concretely, the problem is how to interpret the return type of `foo` below
```scala
// As compiled by e.g. Scala 2.12
class Old {
  def foo(): String = ???
}
```
There are two options:
  - `def foo(): String`
  - `def foo(): String|Null`

The first option is unsound. The second option matches how we handle Java methods.

However, this approach is too-conservative in the presence of generics
```scala
class Old[T] {
  def id(x: T): T = x
}
==>
class Old[T] {
  def id(x: T|Null): T|Null = x
}
```

If we instantiate `Old[T]` with a value type, then `id` now returns a nullable value,
even though it shouldn't:
```scala
val o: Old[Boolean] = ???
val b = o.id(true) // b: Boolean|Null
```

So really the options are between being unsound and being too conservative.
The unsoundness only kicks in if the Scala code being used returns a `null` value.
We hypothesize that `null` is used infrequently in Scala libraries, so we go with
the first option.

If a using an unported Scala library that _produces_ `null`, the user can wrap the
(hopefully rare) API in a type-safe wrapper:
```scala
// Unported library
class Old {
  def foo(): String = null
}

// User code in explicit-null world
def fooWrapper(o: Old): String|Null = o.foo() // ok: String <: String|Null

val o: Old = ???
val s = fooWrapper(o)
```

If the offending API _consumes_ `null`, then the user can cast the null literal to
the right type (the cast will succeed, since at runtime `Null` _is_ a subtype of
any reference type).
```scala
// Unported library
class Old() {
  /** Pass a String, or null to signal a special case */
  def foo(s: String): Unit = ???
}

// User code in explicit-null world
val o: Old = ???
o.foo(null.asInstanceOf[String]) // ok: cast will succeed at runtime
```

## Flow Typing

We added a simple form of flow-sensitive type inference. The idea is that if `p` is a
stable path, then we can know that `p` is non-null if it's compared with the `null` literal.
This information can then be propagated to the `then` and `else` branches of an if-statement (among other places).

Example:
```scala
val s: String|Null = ???
if (s != null) {
  // s: String
}
// s: String|Null
```
A similar inference can be made for the `else` case if the test is `p == null`
```scala
if (s == null) {
  // s: String|Null
} else {
  // s: String
}
```

What exactly is considered a comparison for the purposes of the flow inference?
  - `==` and `!=`
  - `eq` and `ne`

### Non-Stable Paths
If `p` isn't stable, then inferring non-nullness is potentially unsound:
```scala
var s: String|Null = "hello"
if (s != null && {s = null; true}) {
  // s == null
}
```

We _only_ infer non-nullness if `p` is stable (`val`s and not `var`s or `def`s).

### Logical Operators
We also support logical operators (`&&`, `||`, and `!`):
```scala
val s: String|Null = ???
val s2: String|Null = ???
if (s != null && s2 != null) {
  // s: String
  // s2: String
}

if (s == null || s2 == null) {
  // s: String|Null
  // s2: String|Null
} else {
  // s: String
  // s2: String
}
```

### Inside Conditions
We also support type specialization _within_ the condition, taking into account that `&&` and `||` are short-circuiting:
```scala
val s: String|Null
if (s != null && s.length > 0) { // s: String in `s.length > 0`
  // s: String
}

if (s == null || s.length > 0) // s: String in `s.length > 0` {
  // s: String|Null
} else {
  // s: String|Null
}
```

### Unsupported Idioms
We don't support
  - reasoning about non-stable paths
  - flow facts not related to nullability (`if (x == 0) { // x: 0.type not inferred }`)
  - tracking aliasing between non-nullable paths
    ```scala
    val s: String|Null = ???
    val s2: String|Null = ???
    if (s != null && s == s2) {
      // s:  String inferred
      // s2: String not inferred
    }
    ```
