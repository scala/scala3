---
layout: doc-page
title: "Extension Methods"
---

Extension methods allow one to add methods to a type after the type is defined. Example:

```scala
case class Circle(x: Double, y: Double, radius: Double)

extension (c: Circle)
  def circumference: Double = c.radius * math.Pi * 2
```

Like regular methods, extension methods can be invoked with infix `.`:

```scala
val circle = Circle(0, 0, 1)
circle.circumference
```

### Translation of Extension Methods

Extension methods are methods that have a parameter clause in front of the defined identifier.
An extension method named `f` translates to method named `extension_f` that takes the leading parameter section as its first argument list.
So, the definition of `circumference` above translates to the following method, and can also be invoked as such:

```scala
def extension_circumference(c: Circle): Double = c.radius * math.Pi * 2

assert(circle.circumference == extension_circumference(circle))
```

### Operators

The extension method syntax can also be used to define operators. Examples:

```scala
extension (x: String)
  def < (y: String): Boolean = ...
extension (x: Elem)
  def +: (xs: Seq[Elem]): Seq[Elem] = ...
extension (x: Number)
  @infix def min (y: Number): Number = ...

"ab" < "c"
1 +: List(2, 3)
x min 3
```

The three definitions above translate to

```scala
def extension_< (x: String)(y: String): Boolean = ...
def extension_+: (xs: Seq[Elem])(x: Elem): Seq[Elem] = ...
@infix def extension_min(x: Number)(y: Number): Number = ...
```

Note the swap of the two parameters `x` and `xs` when translating
the right-associative operator `+:` to an extension method. This is analogous
to the implementation of right binding operators as normal methods. The Scala
compiler preprocesses an infix operation `x +: xs` to `xs.+:(x)`, so the extension
method ends up being applied to the sequence as first argument (in other words,
the two swaps cancel each other out).

### Generic Extensions

It is also possible to extend generic types by adding type parameters to an extension. For instance:

```scala
extension [T](xs: List[T])
  def second = xs.tail.head

extension [T: Numeric](x: T)
  def + (y: T): T = summon[Numeric[T]].plus(x, y)
```

If an extension method has type parameters, they come immediately after `extension` and are followed by the extended parameter.
When calling a generic extension method, any explicitly given type arguments follow the method name. So the `second` method could be instantiated as follows.

```scala
List(1, 2, 3).second[Int]
```

Of course, the type argument here would usually be left out since it can be inferred.

Extensions can also take using clauses. For instance, the `+` extension above could equivalently be written with a using clause:

```scala
extension [T](x: T)(using n: Numeric[T])
  def + (y: T): T = n.plus(x, y)
```

**Note**: Type parameters have to be given after the `extension` keyword;
they cannot be given after the `def`. This restriction might be lifted in the future once we support multiple type parameter clauses in a method. By contrast, there can be using clauses in front as well as after the `def`.

### Collective Extensions

Sometimes, one wants to define several extension methods that share the same
left-hand parameter type. In this case one can "pull out" the common parameters into
a single extension and enclose all methods in braces or an indented region following a '`:`'.
Example:

```scala
extension (ss: Seq[String]):

  def longestStrings: Seq[String] =
    val maxLength = ss.map(_.length).max
    ss.filter(_.length == maxLength)

  def longestString: String = longestStrings.head
```

Note the right-hand side of `longestString`: it calls `longestStrings` directly, implicitly
assuming the common extended value `ss` as receiver.

Collective extensions like these are a shorthand for individual extensions
where each method is defined separately. For instance, the first extension above expands to

```scala
extension (ss: Seq[String])
  def longestStrings: Seq[String] =
    val maxLength = ss.map(_.length).max
    ss.filter(_.length == maxLength)

extension (ss: Seq[String])
  def longestString: String = ss.longestStrings.head
```

Collective extensions also can take type parameters and have using clauses. Example

```scala
extension [T](xs: List[T])(using Ordering[T]):
  def smallest(n: Int): List[T] = xs.sorted.take(n)
  def smallestIndices(n: Int): List[Int] =
    val limit = smallest(n).max
    xs.zipWithIndex.collect { case (x, i) if x <= limit => i }
```

### Translation of Calls to Extension Methods

To convert a reference to an extension method, the compiler has to know about the extension
method. We say in this case that the extension method is _applicable_ at the point of reference.
There are four possible ways for an extension method to be applicable:

 1. The extension method is visible under a simple name, by being defined or inherited or imported in a scope enclosing the reference.
 2. The extension method is a member of some given
    instance that is visible at the point of the reference.
 3. The reference is of the form `r.m` and the extension method
    is defined in the implicit scope of the type of `r`.
 4. The reference is of the form `r.m` and the extension method
    is defined in some given instance in the implicit scope of the type of `r`.

Here is an example for the first rule:

```scala
trait IntOps:
  extension (i: Int) def isZero: Boolean = i == 0

  extension (i: Int) def safeMod(x: Int): Option[Int] =
    // extension method defined in same scope IntOps
    if x.isZero then None
    else Some(i % x)

object IntOpsEx extends IntOps:
  extension (i: Int) def safeDiv(x: Int): Option[Int] =
    // extension method brought into scope via inheritance from IntOps
    if x.isZero then None
    else Some(i / x)

trait SafeDiv:
  import IntOpsEx._ // brings safeDiv and safeMod into scope

  extension (i: Int) def divide(d: Int) : Option[(Int, Int)] =
     // extension methods imported and thus in scope
    (i.safeDiv(d), i.safeMod(d)) match
      case (Some(d), Some(r)) => Some((d, r))
      case _ => None
```

By the second rule, an extension method can be made available by defining a given instance containing it, like this:

```scala
given ops1 as IntOps // brings safeMod into scope

1.safeMod(2)
```

By the third and fourth rule, an extension method is available if it is in the implicit scope of the receiver type or in a given instance in that scope. Example:

```scala
class List[T]:
  ...
object List:

  extension [T](xs: List[List[T]])
    def flatten: List[T] = xs.foldLeft(Nil: List[T])(_ ++ _)

  given [T: Ordering] as Ordering[List[T]]:
    extension (xs: List[T])
      def < (ys: List[T]): Boolean = ...
end List

// extension method available since it is in the implicit scope of List[List[Int]]
List(List(1, 2), List(3, 4)).flatten

// extension method available since it is in the given Ordering[List[T]],
// which is itself in the implicit scope of List[Int]
List(1, 2) < List(3)
```

The precise rules for resolving a selection to an extension method are as follows.

Assume a selection `e.m[Ts]` where `m` is not a member of `e`, where the type arguments `[Ts]` are optional, and where `T` is the expected type. The following two rewritings are tried in order:

 1. The selection is rewritten to `extension_m[Ts](e)`.
 2. If the first rewriting does not typecheck with expected type `T`,
    and there is an extension method `m` in some eligible object `o`, the selection is rewritten to `o.extension_m[Ts](e)`. An object `o` is _eligible_ if

    - `o` forms part of the implicit scope of `T`, or
    - `o` is a given instance that is visible at the point of the application, or
    - `o` is a given instance in the implicit scope of `T`.

    This second rewriting is attempted at the time where the compiler also tries an implicit conversion
    from `T` to a type containing `m`. If there is more than one way of rewriting, an ambiguity error results.

An extension method can also be used as an identifier by itself. If an identifier `m` does not
resolve, the identifier is rewritten to:

- `x.m`    if the identifier appears in an extension with parameter `x`
- `this.m` otherwise

and the rewritten term is again tried as an application of an extension method. Example:

```scala
extension (s: String)
  def position(ch: Char, n: Int): Int =
    if n < s.length && s(n) != ch then position(ch, n + 1)
    else n
```

The recursive call `position(ch, n + 1)` expands to `s.position(ch, n + 1)` in this case. The whole extension method rewrites to

```scala
def extension_position(s: String)(ch: Char, n: Int): Int =
  if n < s.length && s(n) != ch then extension_position(s)(ch, n + 1)
  else n
```

### More Details

1. To avoid confusion, names of normal methods are not allowed to start with `extension_`.

2. A named import such as `import a.m` of an extension method in `a` will make `m` only available as an extension method.
   To access it under `extension_m` that name as to be imported separately. Example:

   ```scala
   object DoubleOps:
     extension (x: Double) def ** (exponent: Int): Double =
       require(exponent >= 0)
       if exponent == 0 then 1 else x * (x ** (exponent - 1))

   import DoubleOps.{**, extension_**}
   assert(2.0 ** 3 == extension_**(2.0)(3))
   ```

### Syntax

Here are the syntax changes for extension methods and collective extensions relative
to the [current syntax](../../internals/syntax.md).

```
BlockStat         ::=  ... | Extension
TemplateStat      ::=  ... | Extension
TopStat           ::=  ... | Extension
Extension         ::=  ‘extension’ [DefTypeParamClause] ‘(’ DefParam ‘)’
                       {UsingParamClause} ExtMethods
ExtMethods        ::=  ExtMethod | [nl] ‘{’ ExtMethod {semi ExtMethod ‘}’
ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
```

`extension` is a soft keyword. It is recognized as a keyword only if it appears
at the start of a statement and is followed by `[` or `(`. In all other cases
it is treated as an identifier.
