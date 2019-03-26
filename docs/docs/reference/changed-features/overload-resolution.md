---
layout: doc-page
title: "Changes in Overload Resolution"
---

Overload resolution in Dotty improves on Scala 2 in two ways.
First, it takes all argument lists into account instead of
just the first argument list.
Second, it can infer parameter types of function values even if they
are in the first argument list.

## Looking Beyond the First Argument List

Overloading resolution now can take argument lists into account when
choosing among a set of overloaded alternatives.
For example, the following code compiles in Dotty, while it results in an
ambiguous overload error in Scala2:

```scala
  def f(x: Int)(y: String): Int = 0
  def f(x: Int)(y: Int): Int = 0

  f(3)("")     // ok
```

The following code compiles as well:

```scala
  def g(x: Int)(y: Int)(z: Int): Int = 0
  def g(x: Int)(y: Int)(z: String): Int = 0

  g(2)(3)(4)     // ok
  g(2)(3)("")    // ok
```

To make this work, the rules for overloading resolution in section 6.23.3 of the SLS are augmented
as follows:

> In a situation where a function is applied to more than one argument list, if overloading
resolution yields several competing alternatives when `n >= 1` parameter lists are taken
into account, then resolution re-tried using `n + 1` argument lists.

This change is motivated by the new language feature [extension
methods](../contextual/extension-methods.html), where emerges the need to do
overload resolution based on additional argument blocks.

## Parameter Types of Function Values

The handling of function values with missing parameter types has been improved. We can now
pass such values in the first argument list of an overloaded application, provided
that the remaining parameters suffice for picking a variant of the overloaded function.
For example, the following code compiles in Dotty, while it results in an
missing parameter type error in Scala2:
```scala
  def f(x: Int, f: Int => Int) = f(x)
  def f(x: String, f: String => String) = f(x)
  f("a", _.length)
```
To make this work, the rules for overloading resolution in section 6.23.3 of the SLS are modified
as follows:

Replace the sentence

> Otherwise, let `S1,…,Sm` be the vector of types obtained by typing each argument with an undefined expected type.

with the following paragraph:

> Otherwise, let `S1,…,Sm` be the vector of known types of all argument types, where the _known type_ of an argument `E`
is determined as followed:

 - If `E` is a function value `(p_1, ..., p_n) => B` that misses some parameter types, the known type
   of `E` is `(S_1, ..., S_n) => ?`, where each `S_i` is the type of parameter `p_i` if it is given, or `?`
   otherwise. Here `?` stands for a _wildcard type_ that is compatible with every other type.
 - Otherwise the known type of `E` is the result of typing `E` with an undefined expected type.

A pattern matching closure
```scala
  { case P1 => B1 ... case P_n => B_n }
````
is treated as if it was expanded to the function value
```scala
  x => x match { case P1 => B1 ... case P_n => B_n }
```
and is therefore also approximated with a `? => ?` type.
