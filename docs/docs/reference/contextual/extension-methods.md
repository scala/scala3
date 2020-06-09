---
layout: doc-page
title: "Extension Methods"
---

Extension methods allow one to add methods to a type after the type is defined. Example:

```scala
case class Circle(x: Double, y: Double, radius: Double)

def (c: Circle).circumference: Double = c.radius * math.Pi * 2
```

Like regular methods, extension methods can be invoked with infix `.`:

```scala
val circle = Circle(0, 0, 1)
circle.circumference
```

### Translation of Extension Methods

Extension methods are methods that have a parameter clause in front of the defined identifier. 
They translate to functions where the leading parameter section is turned into the first argument list of the function. 
So, the definition of `circumference` above translates to the following function, and can also be invoked as such:

```scala
def circumference(c: Circle): Double = c.radius * math.Pi * 2

assert(circle.circumference == circumference(circle))
```

### Translation of Calls to Extension Methods

When is an extension method applicable? There are two possibilities:

 1. An extension method is applicable if it is visible under a simple name, by being defined
   or inherited or imported in a scope enclosing the application.
 2. An extension method is applicable if it is a member of some given instance at the point of the application.

Here is an example for the first rule:

```scala
trait IntOps {
  def (i: Int).isZero: Boolean = i == 0

  def (i: Int).safeMod(x: Int): Option[Int] =
    // extension method defined in same scope IntOps
    if x.isZero then None
    else Some(i % x)
}

object IntOpsEx extends IntOps {
  def (i: Int).safeDiv(x: Int): Option[Int] =
    // extension method brought into scope via inheritance from IntOps
    if x.isZero then None
    else Some(i / x)
}

trait SafeDiv {
  import IntOpsEx._ // brings safeDiv and safeMod into scope

  def (i: Int) divide(d: Int) : Option[(Int, Int)] =
     // extension methods imported and thus in scope
    (i.safeDiv(d), i.safeMod(d)) match {
      case (Some(d), Some(r)) => Some((d, r))
      case _ => None
    }
}
```

We build up on the above example to outline the second point.
We can make an extension method available by defining a given instance containing it, like this:
```scala
given ops1 as IntOps // brings safeMod into scope

1.safeMod(2)
```

Then `safeMod` is legal everywhere `ops1` is available. Anonymous givens (and any other form of givens) are supported as well:
```scala
given SafeDiv //brings divide into scope (safeMod and safeDiv are not automatically exported)

1.divide(2)
```

The precise rules for resolving a selection to an extension method are as follows.

Assume a selection `e.m[Ts]` where `m` is not a member of `e`, where the type arguments `[Ts]` are optional,
and where `T` is the expected type. The following two rewritings are tried in order:

 1. The selection is rewritten to `m[Ts](e)`.
 2. If the first rewriting does not typecheck with expected type `T`, and there is a given instance `g`
    in either the current scope or in the context scope of `T`, and `g` defines an extension
    method named `m`, then selection is expanded to `g.m[Ts](e)`.
    This second rewriting is attempted at the time where the compiler also tries an implicit conversion
    from `T` to a type containing `m`. If there is more than one way of rewriting, an ambiguity error results.

So `circle.circumference` translates to `CircleOps.circumference(circle)`, provided
`circle` has type `Circle` and `CircleOps` is given (i.e. it is visible at the point of call or it is defined in the companion object of `Circle`).

### Operators

The extension method syntax also applies to the definition of operators.
This case is indicated by omitting the period between the leading parameter list and the operator. In each case the definition syntax mirrors the way the operator is applied.
Examples:
```scala
def (x: String) < (y: String) = ...
def (x: Elem) +: (xs: Seq[Elem]) = ...
def (x: Number) min (y: Number) = ...

"ab" < "c"
1 +: List(2, 3)
x min 3
```
For alphanumeric extension operators like `min` an `@infix` annotation is implied.

<!-- 
TODO: what about @alpha for the non alphanumeric operators, should be required according to
http://dotty.epfl.ch/docs/reference/changed-features/operators.html
"Symbolic methods without @alpha annotations are deprecated"
-->

The three definitions above translate to
```scala
def < (x: String)(y: String) = ...
def +: (xs: Seq[Elem])(x: Elem) = ...
def min(x: Number)(y: Number) = ...
```
Note the swap of the two parameters `x` and `xs` when translating
the right-associative operator `+:` to an extension method. This is analogous
to the implementation of right binding operators as normal methods.


### Generic Extensions

The `IntOps` examples extended a non generic type. 
It is also possible to extend a specific instance of a generic type (e.g. Seq[String] -- see `stringOps` further below).
Moreover, it is also possible to extend generic types by adding type parameters to an extension method. Examples:

```scala
def [T](xs: List[T]) second =
  xs.tail.head

def [T](xs: List[List[T]]) flattened =
  xs.foldLeft[List[T]](Nil)(_ ++ _)

def [T: Numeric](x: T) + (y: T): T =
  summon[Numeric[T]].plus(x, y)
```

If an extension method has type parameters, they come immediately after the `def` and are followed by the extended parameter. 
When calling a generic extension method, any explicitly given type arguments follow the method name. So the `second` method can be instantiated as follows:
```scala
List(1, 2, 3).second[Int]
```
(it's only a showcase, the compiler could of course infer the type).

### Extension Instances

It is quite common to wrap one or more extension methods in a given instance,
in order to make them available as methods without needing to be imported explicitly.
This pattern is supported by a special `extension` syntax. Example:
```scala
extension ops {
  def (xs: Seq[String]).longestStrings: Seq[String] = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
  def (xs: Seq[String]).longestString: String = xs.longestStrings.head
  def [T](xs: List[T]).second: T = xs.tail.head
}
```
An extension instance can only contain extension methods. Other definitions are not allowed. The name `ops`
of the extension is optional. It can be left out:
```scala
extension {
  def (xs: Seq[String]).longestStrings: Seq[String] = ...
  def [T](xs: List[T]).second: T = ...
}
```
If the name of an extension is not explicitly given, it is synthesized from the name and type of the first implemented extension method.

Extension instances map directly to given instances. The `ops` extension above
would expand to
```scala
given ops as AnyRef {
  def (xs: Seq[String]).longestStrings: Seq[String] = ...
  def (xs: Seq[String]).longestString: String = ...
  def [T](xs: List[T]).second: T = ...
}
```
The type "implemented" by this given instance is `AnyRef`, which
is not a type one can summon by itself. This means that the instance can
only be used for its extension methods.

### Collective Extensions

Sometimes, one wants to define several extension methods that share the same
left-hand parameter type. In this case one can "pull out" the common parameters into the extension instance itself. Examples:
```scala
extension stringOps on (ss: Seq[String]) {
  def longestStrings: Seq[String] = {
    val maxLength = ss.map(_.length).max
    ss.filter(_.length == maxLength)
  }
  def longestString: String = longestStrings.head
}

extension listOps on [T](xs: List[T]) {
  def second: T = xs.tail.head
  def third: T = xs.tail.second
}

extension on [T](xs: List[T])(using Ordering[T]) {
  def largest(n: Int) = xs.sorted.takeRight(n)
}
```
**Note**: If a collective extension defines type parameters in its prefix
(as the `listOps` extension above does), the extension methods themselves are not
allowed to have additional type parameters. This restriction might be lifted in the
future once we support multiple type parameter clauses in a method.

Collective extensions like these are a shorthand for extension instances where
the parameters following the `on` are repeated for each implemented method.
For instance, the collective extensions above expand to the following extension instances:
```scala
extension stringOps {
  def (ss: Seq[String]).longestStrings: Seq[String] = {
    val maxLength = ss.map(_.length).max
    ss.filter(_.length == maxLength)
  }
  def (ss: Seq[String]).longestString: String =
    ss.longestStrings.head
}
extension listOps {
  def [T](xs: List[T]).second: T = xs.tail.head
  def [T](xs: List[T]).third: T = xs.tail.second
}
extension {
  def [T](xs: List[T]).largest(using Ordering[T])(n: Int) =
    xs.sorted.takeRight(n)
}
```
One special tweak is shown in the `longestString` method of the `stringOps` extension. It's original definition was
```scala
def longestString: String = longestStrings.head
```
This uses `longestStrings` as an implicit extension method call on the joint
parameter `ss`. The usage is made explicit when translating the method:
```scala
def (ss: Seq[String]).longestString: String =
  ss.longestStrings.head
```
By contrast, the meaning of `this` in a collective extension is as usual
a reference to the enclosing object (i.e. the one implementing the extension methods).
It's not a reference to the shared parameter. So this means that the following
implementation of `longestString` would be illegal:
```scala
def longestString: String = this.longestStrings.head   // error: missing parameter
```
But the following version would again be correct, since it calls the `longestString`
method as a regular non-extension method, passing the prefix parameter `ss` as a regular parameter:
```scala
def longestString: String = this.longestStrings(ss).head
```

### Syntax

Here are the syntax changes for extension methods and collective extensions relative
to the [current syntax](../../internals/syntax.md).
```
DefSig            ::=  ...
                    |  ExtParamClause [nl] [‘.’] id DefParamClauses
ExtParamClause    ::=  [DefTypeParamClause] ‘(’ DefParam ‘)’
TmplDef           ::=  ...
                    |  ‘extension’ ExtensionDef
ExtensionDef      ::=  [id] [‘on’ ExtParamClause {GivenParamClause}] TemplateBody
```
The template body of an extension must consist only of extension method definitions for a regular
extension instance, and only of normal method definitions for a collective extension instance.
It must not be empty.

`extension` and `on` are soft keywords, recognized only when they appear at the start of a
statement in one of the patterns
```scala
extension on ...
extension <ident> on ...
extension { ...
extension <ident> { ...
```
