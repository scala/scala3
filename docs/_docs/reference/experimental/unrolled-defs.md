---
layout: doc-page
title: "Automatic Parameter Unrolling"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/unrolled-defs.html
---

Parameter unrolling enables new parameters to be added to methods and classes,
while still preserving backwards binary compatibility. An `@unroll` annotation, on a parameter with default value, will generate backwards compatible forwarders to a method or constructor.

## Example
```scala
// V1
final def foo(
  s: String,
  i: Int
): String = s + i
```

In the example above, assume version `V1` of a library defines the method `foo` with two parameters: `s` and `i`.
Assume a client library or application `C1` compiles against `V1` of `foo`.

```scala
// V2
final def foo(
  s: String,
  i: Int,
  @unroll b: Boolean = true,
  l: Long = 0L
): String = s + i + b + l

// Generated automatically
`<invisible>` final def foo(
  s: String,
  i: Int
) = foo(s, i, true, 0L)
```

In version `V2`, the library adds the `b` and `l` parameters to `foo`, along with default values.
To preserve compatibility with `V1`, `b` is annotated with `@unroll`, generating a forwarder with only the parameters that come before, i.e. it has the same signature as `foo` in `V1`.

A client `C2` compiling against `V2` will only see `foo` with four parameters in the public API.
The generated forwarder is hidden from those clients.
However, `C1` remains compatible with `V2` of the library, and does not need to be recompiled.
At runtime, it will continue to link against the signature of the old `foo` method, and call the generated forwarder which is accessible in the binary API.

## Specification

### `@unroll` annotation

The `scala.annotation.unroll` annotation can be applied to any term parameter of an effectively-final method:
- `def` in an `object` (i.e. `final` may be omitted)
- `final def` in a `class` or `trait`
- `class` parameters (i.e. primary constructors)
- `def this` in a `class` (i.e. secondary constructors)

### Restrictions

It is illegal for `@unroll` to be applied to any other definition (including `trait` parameters and local methods), or to annotate a type.

`@unroll` may be applied to more than one parameter per method, but all occurrences must appear in the same parameter clause.

The annotated parameter, and any parameters to the right in the same parameter clause, must have a default value.

It is a compile-time error if any generated forwarder matches the signature of another declaration in the same class.

## Code generation

Expansion of `@unroll` parameters is performed before TASTy generation, so generated code will appear in TASTy.

Below specifies the transformations that occur:

For each method `m` of a template, there is a target method `t` which is checked for `@unroll`:
- for `fromProduct`, `copy`, and `apply` of the companion of case class `C`, then `t` is the primary constructor of `C`.
- otherwise `m` is `t`.

if `t` has a single parameter list with `@unroll` annotations, then `m` is subject to code generation. There are two
possible transformations:
1. Forwarder generation
2. Reimplementation: for `fromProduct` of a case class companion

### (1) Forwarder generation

In a method `foo` with unrolled parameters in parameter list `i`:
each parameter `p` with an `@unroll` annotation causes the generation of exactly one forwarder method `f_p`.

for a given method with generic signature

```scala
final def foo[T](ps0...)(psX..., @unroll p, psY...)(psN...): T =
  ...
```
then `f_p` will take the form

```scala
`<invisible>` final def foo[T](ps0...)(psX...)(psN...): T =
  foo(ps0...)(psX..., p_D, psY_D...)(psN...)
```

i.e. result type is preserved, parameter lists before and after `i` are unchanged, and within `i`:
- the parameters `psX...` to the left of `p` are preserved,
- the parameters `p` and `psY...` are dropped.

In the body of `f_p`, parameters are passed positionally to the original `foo`, except for the dropped parameters, which are replaced by default arguments for those parameters (`p_D` for `p`, and `psY_D...` for `psY...`).

Forwarders are generated after type checking, before pickling, and with the `Invisible` flag.
This means that while present in TASTy, they can not be resolved from other top-level classes.

Forwarder method parameters do not have default values, and are never annotated with `@unroll`.

### (2) Method reimplementation

To preserve semantic compatibility of `fromProduct`, its body is replaced with a pattern match over the `productArity` of the parameter.
For each forwarder generated for the case class primary constructor, an equivalent case is generated in the pattern match.

e.g. for a forwarder
```scala
`<invisible>` def this(ps...) = this(ps..., ds...)
```
then the following case is generated:
```scala
case n => new C(...p.productElement(n - 1), ds...)
```
where `n` is an integer matching the number of parameters in `ps`.

The pattern match will have a default wildcard case, which has the same body as the original `fromProduct` method.

In all the complete transformation:

```scala
case class C(ps0...) // ps0 has z parameters

object C:
  def fromProduct(p: Product): C =
    p.productArity match
      case ... => ...
      case n   => new C(...p.productElement(n - 1), ds...)
      case _   => new C(...p.productElement(z - 1))
```


## Background Motivation

The Scala language library ecosystem is based upon compatability of API's represented via both the TASTy format (TASTy compatibility), and the Java class file format (binary compatibility).

Adding a parameter to a method or constructor is a binary backwards incompatible change:
clients compiled against the previous version will expect the old signature to exist, and cause a `LinkageError` to be thrown at runtime.
The correct solution to this problem, to preserve compatibility, is to duplicate the method before adding the new parameter.

In practice, Scala users developed various techniques and disciplines for mitigating this problem when evolving APIs.
Either by forbidding certain features, such as case classes, or various code generation frameworks. Here are some well-known examples:

1. [data-class](https://index.scala-lang.org/alexarchambault/data-class)
2. [SBT Contraband](https://www.scala-sbt.org/contraband/)
3. [Structural Data Structures](https://github.com/scala/docs.scala-lang/pull/2662)

The `@unroll` annotation was proposed as an alternative to these disciplines that not not require learning a new meta-language on top of Scala. The standard data modelling techniques of `def`, `case class`, `enum`, `class` and `trait` are preserved, and the mistake-prone boilerplate is automated.
