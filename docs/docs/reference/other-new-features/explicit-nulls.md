---
layout: doc-page
title: "Explicit Nulls"
---

Explicit nulls is an opt-in feature that modifies the Scala type system, which makes reference types
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

We don't allow the double-equal (`==` and `!=`) and reference (`eq` and `ne`) comparison between
`AnyRef` and `Null` anymore, since a variable with non-nullable type shouldn't have null value.
`null` can only be compared with `Null`, nullable union (`T | Null`), or `Any` type.

For some reason, if we really want to compare `null` with non-null values, we can use cast.

```scala
val x: String = ???
val y: String | Null = ???

x == null       // error: Values of types String and Null cannot be compared with == or !=
x eq null       // error
"hello" == null // error

y == null       // ok
y == x          // ok

(x: String | Null) == null  // ok
(x: Any) == null            // ok
```

## Working with Null

To make working with nullable values easier, we propose adding a few utilities to the standard library.
So far, we have found the following useful:

  - An extension method `.nn` to "cast away" nullability

    ```scala
    def[T] (x: T|Null) nn: x.type & T =
      if (x == null) throw new NullPointerException("tried to cast away nullability, but value is null")
      else x.asInstanceOf[x.type & T]
    ```

    This means that given `x: String|Null`, `x.nn` has type `String`, so we can call all the
    usual methods on it. Of course, `x.nn` will throw a NPE if `x` is `null`.

    Don't use `.nn` on mutable variables directly, which may introduce unknown value into the type.

## Java Interop

The compiler can load Java classes in two ways: from source or from bytecode. In either case,
when a Java class is loaded, we "patch" the type of its members to reflect that Java types
remain implicitly nullable.

Specifically, we patch
* the type of fields
* the argument type and return type of methods

`JavaNull` is an alias for `Null` with magic properties (see below). We illustrate the rules with following examples:

  * The first two rules are easy: we nullify reference types but not value types.

    ```java
    class C {
      String s;
      int x;
    }
    ```
    ==>
    ```scala
    class C {
      val s: String|JavaNull
      val x: Int
    }
    ```

  * We nullify type parameters because in Java a type parameter is always nullable, so the following code compiles.

    ```java
    class C<T> { T foo() { return null; } }
    ```
    ==>
    ```scala
    class C[T] { def foo(): T|JavaNull }
    ```

    Notice this is rule is sometimes too conservative, as witnessed by

    ```scala
    class InScala {
      val c: C[Bool] = ???  // C as above
      val b: Bool = c.foo() // no longer typechecks, since foo now returns Bool|Null
    }
    ```

  * This reduces the number of redundant nullable types we need to add. Consider

    ```java
    class Box<T> { T get(); }
    class BoxFactory<T> { Box<T> makeBox(); }
    ```
    ==>
    ```scala
    class Box[T] { def get(): T|JavaNull }
    class BoxFactory[T] { def makeBox(): Box[T]|JavaNull }
    ```

    Suppose we have a `BoxFactory[String]`. Notice that calling `makeBox()` on it returns a
    `Box[String]|JavaNull`, not a `Box[String|JavaNull]|JavaNull`. This seems at first
    glance unsound ("What if the box itself has `null` inside?"), but is sound because calling
    `get()` on a `Box[String]` returns a `String|JavaNull`.

    Notice that we need to patch _all_ Java-defined classes that transitively appear in the
    argument or return type of a field or method accessible from the Scala code being compiled.
    Absent crazy reflection magic, we think that all such Java classes _must_ be visible to
    the Typer in the first place, so they will be patched.

  * We will append `JavaNull` to the type arguments if the generic class is defined in Scala.

    ```java
    class BoxFactory<T> {
      Box<T> makeBox(); // Box is Scala-defined
      List<Box<List<T>>> makeCrazyBoxes(); // List is Java-defined
    }
    ```
    ==>
    ```scala
    class BoxFactory[T] {
      def makeBox(): Box[T | JavaNull] | JavaNull
      def makeCrazyBoxes(): List[Box[List[T] | JavaNull]] | JavaNull
    }
    ```

    In this case, since `Box` is Scala-defined, and we will get `Box[T|JavaNull]|JavaNull`.
    This is needed because our nullability function is only applied (modularly) to the Java
    classes, but not to the Scala ones, so we need a way to tell `Box` that it contains a
    nullable value.

    The `List` is Java-defined, so we don't append `JavaNull` to its type argument. But we
    still need to nullify its inside.

  * We don't nullify _simple_ literal constant (`final`) fields, since they are known to be non-null

    ```java
    class Constants {
      final String NAME = "name";
      final int AGE = 0;
      final char CHAR = 'a';

      final String NAME_GENERATED = getNewName();
    }
    ```
    ==>
    ```scala
    class Constants {
      val NAME: String("name") = "name"
      val AGE: Int(0) = 0
      val CHAR: Char('a') = 'a'

      val NAME_GENERATED: String | Null = ???
    }
    ```

  * We don't append `JavaNull` to a field and the return type of a method which is annotated with a
    `NotNull` annotation.

    ```java
    class C {
      @NotNull String name;
      @NotNull List<String> getNames(String prefix); // List is Java-defined
      @NotNull Box<String> getBoxedName(); // Box is Scala-defined
    }
    ```
    ==>
    ```scala
    class C {
      val name: String
      def getNames(prefix: String | JavaNull): List[String] // we still need to nullify the paramter types
      def getBoxedName(): Box[String | JavaNull] // we don't append `JavaNull` to the outmost level, but we still need to nullify inside
    }
    ```

    The annotation must be from the list below to be recognized as NotNull by the compiler.
    Check `Definitions.scala` for an updated list.

    ```scala
    // A list of annotations that are commonly used to indicate that a field/method argument or return
    // type is not null. These annotations are used by the nullification logic in JavaNullInterop to
    // improve the precision of type nullification.
    // We don't require that any of these annotations be present in the class path, but we want to
    // create Symbols for the ones that are present, so they can be checked during nullification.
    @tu lazy val NotNullAnnots: List[ClassSymbol] = ctx.getClassesIfDefined(
      "javax.annotation.Nonnull" ::
      "edu.umd.cs.findbugs.annotations.NonNull" ::
      "androidx.annotation.NonNull" ::
      "android.support.annotation.NonNull" ::
      "android.annotation.NonNull" ::
      "com.android.annotations.NonNull" ::
      "org.eclipse.jdt.annotation.NonNull" ::
      "org.checkerframework.checker.nullness.qual.NonNull" ::
      "org.checkerframework.checker.nullness.compatqual.NonNullDecl" ::
      "org.jetbrains.annotations.NotNull" ::
      "lombok.NonNull" ::
      "io.reactivex.annotations.NonNull" :: Nil map PreNamedString)
    ```

### JavaNull

To enable method chaining on Java-returned values, we have the special type alias for `Null`:

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

## Flow Typing

We added a simple form of flow-sensitive type inference. The idea is that if `p` is a
stable path or a trackable variable, then we can know that `p` is non-null if it's compared
with the `null`. This information can then be propagated to the `then` and `else` branches
of an if-statement (among other places).

Example:

```scala
val s: String|Null = ???
if (s != null) {
  // s: String
}
// s: String|Null

assert(x != null)
// s: String
```

A similar inference can be made for the `else` case if the test is `p == null`

```scala
if (s == null) {
  // s: String|Null
} else {
  // s: String
}
```

`==` and `!=` is considered a comparison for the purposes of the flow inference.

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
val s: String|Null = ???

if (s != null && s.length > 0) { // s: String in `s.length > 0`
  // s: String
}

if (s == null || s.length > 0) // s: String in `s.length > 0` {
  // s: String|Null
} else {
  // s: String|Null
}
```

### Match Case

The non-null cases can be detected in match statements.

```scala
val s: String|Null = ???

s match {
  case _: String => // s: String
  case _ =>
}
```

### Mutable Variable

We are able to detect the nullability of some local mutable variables. A simple example is:

```scala
class C(val x: Int, val next: C|Null)

var xs: C|Null = C(1, C(2, null))
// xs is trackable, since all assignments are in the same mathod
while (xs != null) {
  // xs: C
  val xsx: Int = xs.x
  val xscpy: C = xs
  xs = xscpy // since xscpy is non-null, xs still has type C after this line
  // xs: C
  xs = xs.next // after this assignment, xs can be null again
  // xs: C | Null
}
```

When dealing with local mutable variables, there are two questions:

1. Whether to track a local mutable variable during flow typing.
 We track a local mutable variable iff the variable is not assigned in a closure.
 For example, in the following code `x` is assigned to by the closure `y`, so we do not
 do flow typing on `x`.

 ```scala
 var x: String|Null = ???
 def y = {
   x = null
 }
 if (x != null) {
   // y can be called here, which break the fact
   val a: String = x // error: x is captured and mutated by the closure, not trackable
 }
 ```

2. Whether to generate and use flow typing on a specific _use_ of a local mutable variable.
 We only want to do flow typing on a use that belongs to the same method as the definition
 of the local variable.
 For example, in the following code, even `x` is not assigned to by a closure, but we can only
 use flow typing in one of the occurrences (because the other occurrence happens within a nested
 closure).

 ```scala
 var x: String|Null = ???
 def y = {
   if (x != null) {
     // not safe to use the fact (x != null) here
     // since y can be executed at the same time as the outer block
     val _: String = x
   }
 }
 if (x != null) {
   val a: String = x // ok to use the fact here
   x = null
 }
 ```

See more examples in `tests/explicit-nulls/neg/var-ref-in-closure.scala`.

Currently, we are unable to track paths with a mutable variable prefix.
For example, `x.a` if `x` is mutable.

### Unsupported Idioms

We don't support:

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

## Binary Compatibility

Our strategy for binary compatibility with Scala binaries that predate explicit nulls
and new libraries compiled without `-Yexplicit-nulls` is to leave the types unchanged
and be compatible but unsound.

[More details](../../internals/explicit-nulls.md)
