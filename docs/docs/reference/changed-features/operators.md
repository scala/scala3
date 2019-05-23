---
layout: doc-page
title: Rules for Operators
---

There are two annotations that regulate operators: `infix` and `alpha`.

## The @alpha Annotation

An `@alpha` annotation on a method definition defines an alternate name for the implementation of that method: Example:
```scala
object VecOps {
  @alpha("append") def (xs: Vec[T]) ++= [T] (ys: Vec[T]): Vec[T] = ...
}
```
Here, the `++=` operation is implemented (in Byte code or native code) under the name `append`. The implementation name affects the code that is generated, and is the name under which code from other languages can call the method. For instance, `++=` could be invoked from Java like this:
```
VecOps.append(vec1, vec2)
```
The `@alpha` annotation has no bearing on Scala usages. Any application of that method in Scala has to use `++=`, not `append`.

An `@alpha` annotation will be _mandatory_ if the method name is symbolic. Symbolic methods without `@alpha` annotations are deprecated.

### Motivation

The `@alpha` annotation serves a dual purpose:

 - It helps interoperability between Scala and other languages.
 - It serves as a documentation tool by providing an alternative regular name
   as an alias of a symbolic operator.

### Details

 1. `@alpha` is defined in package `scala.annotation`. It takes a single argument
    of type `String`. That string is called the _external name_ of the definition
    that's annotated.

 2. An `@alpha` annotation can be given for all kinds of definitions.

 3. The name given in an `@alpha` annotation must be a legal name
    for the defined entities on the host platform.

 4. Definitions with symbolic names should have an `@alpha` annotation. Lack of such
    an annotation will raise a deprecation warning.

 5. Definitions with names in backticks that are not legal host platform names
    should have an `@alpha` annotation. Lack of such an annotation will raise a deprecation warning.

 6. @alpha annotations must agree: If two definitions are members of an object or class with the same name and matching types, then either none of them has an `@alpha` annotation, or both have `@alpha` annotations with the same name.

 7. There must be a one-to-one relationship between external and internal names:
 If two definitions are members of an object or class with matching types and both have `@alpha` annotations with the same external name, then their internal method names must also be the same.

## The @infix Annotation

An `@infix` annotation on a method definition allows using the method as an infix operation. Example:
```scala
trait MultiSet[T] {

  @infix
  def union(other: MultiSet[T]): MultiSet[T]

  def difference(other: MultiSet[T]): MultiSet[T]

  @alpha("intersection")
  def *(other: MultiSet[T]): MultiSet[T]
}

val s1, s2: MultiSet[Int]

s1 union s2         // OK
s1.union(s2)        // also OK

s1.difference(s2)   // OK
s1 `difference` s2  // OK
s1 difference s2    // gives a deprecation warning

s1 * s2             // OK
s1.*(s2)            // also OK, but unusual
```
Infix operations involving alphanumeric operators are deprecated, unless
one of the following conditions holds:

 - the operator definition carries an `@infix` annotation, or
 - the operator was compiled with Scala 2, or
 - the operator is followed by an opening brace.

An alphanumeric operator is an operator consisting entirely of letters, digits, the `$` and `_` characters, or
any unicode character `c` for which `java.lang.Character.isIdentifierPart(c)` returns `true`.

Infix operations involving symbolic operators are always allowed, so `@infix` is redundant for methods with symbolic names.

The @infix annotation can also be given to a type:
```
@infix type or[X, Y]
val x: String or Int = ...
```

### Motivation

The purpose of the `@infix` annotation is to achieve consistency across a code base in how a method or type is applied. The idea is that the author of a method decides whether that method should be applied as an infix operator or in a regular application. Use sites then implement that decision consistently.

### Details

 1. `@infix` is defined in package `scala.annotation`.

 2. If a method overrides another, their infix annotations must agree. Either both are annotated with `@infix`, or none of them are.

 3. `@infix` annotations can be given to method definitions. The first non-receiver parameter list of an `@infix` method must define exactly one parameter. Examples:

      ```scala
      @infix def op(x: S): R                  // ok
      @infix def op[T](x: T)(y: S): R         // ok
      @infix def op[T](x: T, y: S): R         // error: two parameters

      @infix def (x: A) op (y: B): R          // ok
      @infix def (x: A) op (y1: B, y2: B): R  // error: two parameters
      ```

 4. @infix annotations can also be given to type, trait or class definitions that have exactly two type parameters. An infix type like

      ```scala
      @infix type op[X, Y]
      ```

    can be applied using infix syntax, i.e. `A op B`.

 5. To smooth migration to Scala 3.0, alphanumeric operations will only be deprecated from Scala 3.1 onwards,
or if the `-strict` option is given in Dotty/Scala 3.
