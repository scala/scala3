---
title: Basic Definitions
layout: default
chapter: 4
---

# Basic Definitions

```ebnf
PatVarDef   ::=  ‘val’ PatDef
              |  ‘var’ VarDef
Def         ::=  PatVarDef
              |  ‘def’ FunDef
              |  ‘type’ {nl} TypeDef
              |  ‘opaque‘ ‘type‘ {nl} OpaqueTypeDef
              |  TmplDef
```

A _definition_ introduces names that denote terms and assigns them types, or that denote types and assigns them [type definitions](./03-types.html#type-definitions).
It can form part of an object or [class definition](05-classes-and-objects.html#templates) or it can be local to a block.

The scope of a name introduced by a definition is the whole statement sequence containing the definition.
However, there is a restriction on forward references in blocks:
In a statement sequence ´s_1 ... s_n´ making up a block, if a simple name in ´s_i´ refers to an entity defined by ´s_j´ where ´j \geq i´, then for all ´s_k´ between and including ´s_i´ and ´s_j´,

- ´s_k´ cannot be a variable definition.
- If ´s_k´ is a value definition, it must be lazy.

Moreover, in a block, all term definitions must be concrete, and opaque type alias definitions are not allowed.

<!--
Every basic definition may introduce several defined names, separated
by commas. These are expanded according to the following scheme:
\bda{lcl}
\VAL;x, y: T = e && \VAL; x: T = e \\
                 && \VAL; y: T = x \\[0.5em]

\VAR;x, y: T := e && \VAR;x: T := e\\
                  && \VAR;y: T := x\\[0.5em]
\eda

The variable definition `var x, y: Int`
expands to `var x: Int; var y: Int`.

The value definition `val x, y: Int = 1`
expands to `val x: Int = 1; val y: Int = 1`.
-->

## Value Definitions

```ebnf
PatVarDef    ::=  ‘val’ PatDef
PatDef       ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] [‘=’ Expr]
ids          ::=  id {‘,’ id}
```

An abstract value definition `val ´x´: ´T´` introduces ´x´ as a name of a value of _declared type_ ´T´.
´T´ must be explicitly specified and must be a [proper type](03-types.html#proper-types).

A concrete value definition `val ´x´: ´T´ = ´e´` defines ´x´ as a name of the value that results from the evaluation of ´e´.
If the value definition is not recursive, the declared type ´T´ may be omitted, in which case the [packed type](06-expressions.html#expression-typing) of the expression ´e´ is assumed.
If a type ´T´ is given, then it must be a [proper type](03-types.html#proper-types) and ´e´ is expected to [conform to it](06-expressions.html#expression-typing).

Evaluation of the value definition implies evaluation of its right-hand side ´e´, unless it has the modifier `lazy`.
The effect of the value definition is to bind ´x´ to the value of ´e´ converted to type ´T´.
A `lazy` value definition evaluates its right hand side ´e´ the first time the value is accessed.

A _constant value definition_ is of the form

```scala
final val x = e
```

where `e` is a [constant expression](06-expressions.html#constant-expressions).
The `final` modifier must be present and no type annotation may be given.
References to the constant value `x` are themselves treated as constant expressions; in the generated code they are replaced by the definition's right-hand side `e`.

Concrete value definitions can alternatively have a [pattern](08-pattern-matching.html#patterns) as left-hand side.
If ´p´ is some pattern other than a simple name or a name followed by a colon and a type, then the value definition `val ´p´ = ´e´` is expanded as follows:

1. If the pattern ´p´ has bound variables ´x_1, ..., x_n´, where ´n > 1´:

```scala
val ´\$x´ = ´e´ match {case ´p´ => (´x_1, ..., x_n´)}
val ´x_1´ = ´\$x´._1
...
val ´x_n´ = ´\$x´._n
```

Here, ´\$x´ is a fresh name.

2. If ´p´ has a unique bound variable ´x´:

```scala
val ´x´ = ´e´ match { case ´p´ => ´x´ }
```

3. If ´p´ has no bound variables:

```scala
´e´ match { case ´p´ => () }
```

###### Example

The following are examples of value definitions

```scala
val foo: Int              // abstract value definition
val pi = 3.1415
val pi: Double = 3.1415   // equivalent to first definition
val Some(x) = f()         // a pattern definition
val x :: xs = mylist      // an infix pattern definition
```

The last two definitions have the following expansions.

```scala
val x = f() match { case Some(x) => x }

val x´\$´ = mylist match { case x :: xs => (x, xs) }
val x = x´\$´._1
val xs = x´\$´._2
```

The name of any defined value may not end in `_=`.

A value definition `val ´x_1, ..., x_n´: ´T´` is a shorthand for the sequence of value definitions `val ´x_1´: ´T´; ...; val ´x_n´: ´T´`.
A value definition `val ´p_1, ..., p_n´ = ´e´` is a shorthand for the sequence of value definitions `val ´p_1´ = ´e´; ...; val ´p_n´ = ´e´`.
A value definition `val ´p_1, ..., p_n: T´ = ´e´` is a shorthand for the sequence of value definitions `val ´p_1: T´ = ´e´; ...; val ´p_n: T´ = ´e´`.

## Variable Definitions

```ebnf
Dcl            ::=  ‘var’ VarDcl
PatVarDef      ::=  ‘var’ VarDef
VarDcl         ::=  ids ‘:’ Type
VarDef         ::=  PatDef
                 |  ids ‘:’ Type ‘=’ ‘_’
```

An abstract variable definition `var ´x´: ´T´` is equivalent to the definition of both a _getter method_ ´x´ *and* a _setter method_ `´x´_=`:

```scala
def ´x´: ´T´
def ´x´_= (´y´: ´T´): Unit
```

An implementation of a class may implement a defined abstract variable using a concrete variable definition, or by defining the corresponding setter and getter methods.

A concrete variable definition `var ´x´: ´T´ = ´e´` introduces a mutable variable with type ´T´ and initial value as given by the expression ´e´.
The type ´T´ can be omitted, in which case the type of ´e´ is assumed.
If ´T´ is given, then it must be a [proper type](03-types.html#proper-types) and ´e´ is expected to [conform to it](06-expressions.html#expression-typing).

Variable definitions can alternatively have a [pattern](08-pattern-matching.html#patterns) as left-hand side.
A variable definition  `var ´p´ = ´e´` where ´p´ is a pattern other than a simple name or a name followed by a colon and a type is expanded in the same way as a [value definition](#value-definitions) `val ´p´ = ´e´`, except that the free names in ´p´ are introduced as mutable variables, not values.

The name of any defined variable may not end in `_=`.

The right-hand-side of a mutable variable definition that is a member of a template can be the special reference `scala.compiletime.uninitialized`: `var ´x´: ´T´ = scala.compiletime.uninitialized`.
It introduces a mutable field with type ´T´ and a default initial value.
The default value depends on the type ´T´ as follows:

| default  | type ´T´                           |
|----------|------------------------------------|
|`0`       | `Int` or one of its subrange types |
|`0L`      | `Long`                             |
|`0.0f`    | `Float`                            |
|`0.0d`    | `Double`                           |
|`false`   | `Boolean`                          |
|`()`      | `Unit`                             |
|`null`    | all other types                    |

`scala.compiletime.uninitialized` can never appear anywhere else.
For compatibility with Scala 2, the syntax `var ´x´: ´T´ = _` is accepted as equivalent to using `uninitialized`.

When they occur as members of a template, both forms of concrete variable definition also introduce a setter method `´x´_=` which changes the value currently assigned to the variable.
The setter has the same signatures as for an abstract variable definition.
It is then not possible to directly modify the value assigned to the variable; mutations always go through the corresponding setter.

###### Example

The following example shows how _properties_ can be simulated in Scala.
It defines a class `TimeOfDayVar` of time values with updatable integer fields representing hours, minutes, and seconds.
Its implementation contains tests that allow only legal values to be assigned to these fields.
The user code, on the other hand, accesses these fields just like normal variables.

```scala
class TimeOfDayVar {
  private var h: Int = 0
  private var m: Int = 0
  private var s: Int = 0

  def hours              =  h
  def hours_= (h: Int)   =  if (0 <= h && h < 24) this.h = h
                            else throw new DateError()

  def minutes            =  m
  def minutes_= (m: Int) =  if (0 <= m && m < 60) this.m = m
                            else throw new DateError()

  def seconds            =  s
  def seconds_= (s: Int) =  if (0 <= s && s < 60) this.s = s
                            else throw new DateError()
}
val d = new TimeOfDayVar
d.hours = 8; d.minutes = 30; d.seconds = 0
d.hours = 25                  // throws a DateError exception
```

A variable definition `var ´x_1, ..., x_n´: ´T´` is a shorthand for the sequence of variable definitions `var ´x_1´: ´T´; ...; var ´x_n´: ´T´`.
A variable definition `var ´x_1, ..., x_n´ = ´e´` is a shorthand for the sequence of variable definitions `var ´x_1´ = ´e´; ...; var ´x_n´ = ´e´`.
A variable definition `var ´x_1, ..., x_n: T´ = ´e´` is a shorthand for the sequence of variable definitions `var ´x_1: T´ = ´e´; ...; var ´x_n: T´ = ´e´`.

## Type Member Definitions

```ebnf
Dcl             ::=  ‘type’ {nl} TypeDcl
TypeDcl         ::=  id [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
Def             ::=  ‘type’ {nl} TypeDef
                  |  ‘opaque‘ ‘type‘ {nl} OpaqueTypeDef
TypeDef         ::=  id [TypeParamClause] ‘=’ Type
OpaqueTypeDef   ::=  id [TypeParamClause] [‘>:’ Type] [‘<:’ Type] ‘=’ Type
```

_Type members_ can be abstract type members, type aliases, or opaque type aliases.

A possibly parameterized _abstract type member_ definition `type ´t´[´\mathit{tps}\,´] >: ´L´ <: ´H´` declares ´t´ to be an abstract type.
If omitted, ´L´ and ´H´ are implied to be `Nothing` and `scala.Any`, respectively.

A possibly parameterized _type alias_ definition `type ´t´[´\mathit{tps}\,´] = ´T´` defines ´t´ to be a concrete type member.

A possibly parameterized _opaque type alias_ definition `opaque type ´t´[´\mathit{tps}\,´] >: ´L´ <: ´H´ = ´T´` defines ´t´ to be an opaque type alias with public bounds `>: ´L´ <: ´H´` and a private alias `= ´T´`.

If a type parameter clause `[´\mathit{tps}\,´]` is present, it is desugared away according to the rules in the following section.

### Desugaring of parameterized type definitions

A parameterized type definition is desugared into an unparameterized type definition whose bounds are [type lambdas](03-types.html#type-lambdas) with explicit variance annotations.

The scope of a type parameter extends over the bounds `>: ´L´ <: ´U´` or the alias `= ´T´` and the type parameter clause ´\mathit{tps}´ itself.
A higher-order type parameter clause (of an abstract type constructor ´tc´) has the same kind of scope, restricted to the definition of the type parameter ´tc´.

To illustrate nested scoping, these definitions are all equivalent: `type t[m[x] <: Bound[x], Bound[x]]`, `type t[m[x] <: Bound[x], Bound[y]]` and `type t[m[x] <: Bound[x], Bound[_]]`, as the scope of, e.g., the type parameter of ´m´ is limited to the definition of ´m´.
In all of them, ´t´ is an abstract type member that abstracts over two type constructors: ´m´ stands for a type constructor that takes one type parameter and that must be a subtype of `Bound`, ´t´'s second type constructor parameter.
`t[MutableList, Iterable]` is a valid use of ´t´.

#### Abstract Type

A parameterized abstract type
```scala
type ´t´[´\mathit{tps}\,´] >: ´L´ <: ´H´
```
is desugared into an unparameterized abstract type as follows:
- If `L` conforms to `Nothing`, then,

  ```scala
type ´t´ >: Nothing
       <: [´\mathit{tps'}\,´] =>> ´H´
  ```
- otherwise,

  ```scala
type ´t´ >: [´\mathit{tps'}\,´] =>> ´L´
       <: [´\mathit{tps'}\,´] =>> ´H´
  ```

If at least one of the ´\mathit{tps}´ contains an explicit variance annotation, then ´\mathit{tps'} = \mathit{tps}´, otherwise we infer the variance of each type parameter as with the user-written type lambda `[´\mathit{tps}\,´] =>> ´H´`.

The same desugaring applies to type parameters.
For instance,
```scala
[F[X] <: Coll[X]]
```
is treated as a shorthand for
```scala
[F >: Nothing <: [X] =>> Coll[X]]
```

#### Type Alias

A parameterized type alias
```scala
type ´t´[´\mathit{tps}\,´] = ´T´
```
is desugared into an unparameterized type alias
```scala
type ´t´ = [´\mathit{tps'}\,´] =>> ´T´
```
where ´\mathit{tps'}´ is computed as in the previous case.

#### Opaque Type Alias

A parameterized type alias
```scala
type ´t´[´\mathit{tps}\,´] >: ´L´ <: ´H´ = ´T´
```
is desugared into an unparameterized opaque type alias as follows:
- If `L` conforms to `Nothing`, then,

  ```scala
type ´t´ >: Nothing <: [´\mathit{tps'}\,´] =>> ´H´ = [´\mathit{tps'}\,´] =>> ´T´
  ```
- otherwise,

  ```scala
type ´t´ >: [´\mathit{tps'}\,´] =>> ´L´ <: [´\mathit{tps'}\,´] =>> ´H´ = [´\mathit{tps'}\,´] =>> ´T´
  ```
where ´\mathit{tps'}´ is computed as in the previous cases.

### Non-Parameterized Type Member Definitions

An _abstract type member_ definition `type ´t´ >: ´L´ <: ´H´` declares ´t´ to be an abstract type whose [type definition](03-types.html#type-definitions) has the lower bound type ´L´ and upper bound type ´H´.

If a type definition appears as a member definition of a type, implementations of the type may implement ´t´ with any type ´T´ for which ´L <: T <: H´.
It is a compile-time error if ´L´ does not conform to ´H´.

A _type alias_ definition `type ´t´ = ´T´` defines ´t´ to be an alias name for the type ´T´.

An _opaque type alias_ definition `opaque type ´t´ >: ´L´ <: ´H´ = ´T´` defines ´t´ to be an opaque type alias with public bounds `>: ´L´ <: ´H´` and a private alias `= ´T´`.
An opaque type alias can only be declared within a [template](./05-classes-and-objects.html#templates).
It cannot be `private` and cannot be overridden in subclasses.
In order for the definition to be valid, ´T´ must satisfy some constraints:

- ´L <: T´ and ´T <: H´ must be true,
- ´T´ must not be a context function type, and
- If ´T´ is a type lambda, its result must be a proper type (i.e., it cannot be a curried type lambda).

When viewed from within its enclosing template, an opaque type alias behaves as a type alias with type definition `= ´T´`.
When viewed from anywhere else, it behaves as an abstract type member with type definition `>: ´L´ <: ´H´`.
See [`memberType`](./03-types.html#member-type) for the precise mechanism that governs this dual view.

The scope rules for [definitions](#basic-definitions) and [type parameters](#method-definitions) make it possible that a type name appears in its own bounds or in its right-hand side.
However, it is a static error if a type alias refers recursively to the defined type itself.
That is, the type ´T´ in a type alias `type ´t´[´\mathit{tps}\,´] = ´T´` may not refer directly or indirectly to the name ´t´.
It is also an error if an abstract type is directly or indirectly its own upper or lower bound.

###### Example

The following are legal type definitions:

```scala
type IntList = List[Integer]
type T <: Comparable[T]
type Two[A] = Tuple2[A, A] // desugars to Two = [A] =>> Tuple2[A, A]
type MyCollection[+X] <: Iterable[X] // desugars to MyCollection <: [+X] =>> Iterable[X]
```

The following are illegal:

```scala
type Abs = Comparable[Abs]      // recursive type alias

type S <: T                     // S, T are bounded by themselves.
type T <: S

type T >: Comparable[T.That]    // Cannot select from T.
                                // T is a type, not a value
type MyCollection <: Iterable   // The reference to the type constructor
                                // Iterable must explicitly state its type arguments.
```

If a type alias `type ´t´ = ´S´` refers to a class type ´S´ (or to a type lambda that is the eta-expansion of class type ´S´), the name ´t´ can also be used as a constructor for objects of type ´S´.

###### Example

Suppose we make `Pair` an alias of the parameterized class `Tuple2`, as follows:

```scala
type Pair[+A, +B] = Tuple2[A, B]
object Pair {
  def apply[A, B](x: A, y: B) = Tuple2(x, y)
  def unapply[A, B](x: Tuple2[A, B]): Option[Tuple2[A, B]] = Some(x)
}
```

As a consequence, for any two types ´S´ and ´T´, the type `Pair[´S´, ´T\,´]` is equivalent to the type `Tuple2[´S´, ´T\,´]`.
`Pair` can also be used as a constructor instead of `Tuple2`, as in:

```scala
val x: Pair[Int, String] = new Pair(1, "abc")
```

## Type Parameters

```ebnf
TypeParamClause  ::= ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
VariantTypeParam ::= {Annotation} [‘+’ | ‘-’] TypeParam
TypeParam        ::= (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type] [‘:’ Type]
```

Type parameters appear in type definitions, class definitions, and method definitions.
In this section we consider only type parameter definitions with lower bounds `>: ´L´` and upper bounds `<: ´U´` whereas a discussion of context bounds `: ´U´` and view bounds `<% ´U´` is deferred to [here](07-implicits.html#context-bounds-and-view-bounds).

The most general form of a proper type parameter is
`´@a_1 ... @a_n´ ´\pm´ ´t´ >: ´L´ <: ´U´`.
Here, ´L´, and ´U´ are lower and upper bounds that constrain possible type arguments for the parameter.
It is a compile-time error if ´L´ does not conform to ´U´.
´\pm´ is a _variance_, i.e. an optional prefix of either `+`, or `-`. One or more annotations may precede the type parameter.

<!--
The upper bound ´U´ in a type parameter clauses may not be a final
class. The lower bound may not denote a value type.

TODO: Why
-->

<!--
TODO: this is a pretty awkward description of scoping and distinctness of binders
-->

The names of all type parameters must be pairwise different in their enclosing type parameter clause.
The scope of a type parameter includes in each case the whole type parameter clause.
Therefore it is possible that a type parameter appears as part of its own bounds or the bounds of other type parameters in the same clause.
However, a type parameter may not be bounded directly or indirectly by itself.

A type constructor parameter adds a nested type parameter clause to the type parameter.
The most general form of a type constructor parameter is `´@a_1 ... @a_n \pm t[\mathit{tps}\,]´ >: ´L´ <: ´U´`.

The above scoping restrictions are generalized to the case of nested type parameter clauses, which declare higher-order type parameters.
Higher-order type parameters (the type parameters of a type parameter ´t´) are only visible in their immediately surrounding parameter clause (possibly including clauses at a deeper nesting level) and in the bounds of ´t´.
Therefore, their names must only be pairwise different from the names of other visible parameters.
Since the names of higher-order type parameters are thus often irrelevant, they may be denoted with a `‘_’`, which is nowhere visible.

###### Example
Here are some well-formed type parameter clauses:

```scala
[S, T]
[@specialized T, U]
[Ex <: Throwable]
[A <: Comparable[B], B <: A]
[A, B >: A, C >: A <: B]
[M[X], N[X]]
[M[_], N[_]] // equivalent to previous clause
[M[X <: Bound[X]], Bound[_]]
[M[+X] <: Iterable[X]]
```

The following type parameter clauses are illegal:

```scala
[A >: A]                  // illegal, `A' has itself as bound
[A <: B, B <: C, C <: A]  // illegal, `A' has itself as bound
[A, B, C >: A <: B]       // illegal lower bound `A' of `C' does
                          // not conform to upper bound `B'.
```

## Variance Annotations

Variance annotations indicate how instances of parameterized types vary with respect to [subtyping](03-types.html#conformance).
A ‘+’ variance indicates a covariant dependency, a ‘-’ variance indicates a contravariant dependency, and a missing variance indication indicates an invariant dependency.

A variance annotation constrains the way the annotated type variable may appear in the type or class which binds the type parameter.
In a type definition `type ´T´[´\mathit{tps}\,´] = ´S´`, `type ´T´[´\mathit{tps}\,´] >: ´L´ <: ´U´` or `opaque type ´T´[´\mathit{tps}\,´] >: ´L´ <: ´U´ = ´S´`, type parameters labeled ‘+’ must only appear in covariant position whereas type parameters labeled ‘-’ must only appear in contravariant position.
Analogously, for a class definition `class ´C´[´\mathit{tps}\,´](´\mathit{ps}\,´) extends ´T´ { ´x´: ´S´ => ...}`, type parameters labeled ‘+’ must only appear in covariant position in the self type ´S´ and the template ´T´, whereas type parameters labeled ‘-’ must only appear in contravariant position.

The variance position of a type parameter in a type or template is defined as follows.
Let the opposite of covariance be contravariance, and the opposite of invariance be itself.
The top-level of the type or template is always in covariant position.
The variance position changes at the following constructs.

- The variance position of a method parameter is the opposite of the variance position of the enclosing parameter clause.
- The variance position of a type parameter is the opposite of the variance position of the enclosing type parameter clause.
- The variance position of the lower bound of a type definition or type parameter is the opposite of the variance position of the type definition or parameter.
- The type of a mutable variable is always in invariant position.
- The right-hand side of a type alias is always in invariant position.
- The prefix ´p´ of a type selection `´p.T´` is always in invariant position.
- For a type argument ´T´ of a type `´S´[´..., T, ...´]`:
  - If the corresponding type parameter of ´S´ is invariant, then ´T´ is in invariant position.
  - If the corresponding type parameter of ´S´ is contravariant, the variance position of ´T´ is the opposite of the variance position of the enclosing type `´S´[´..., T, ...´]`.

References to the type parameters in [object-private values, types, variables, or methods](05-classes-and-objects.html#modifiers) of the class are not checked for their variance position.
In these members the type parameter may appear anywhere without restricting its legal variance annotations.

###### Example
The following variance annotation is legal.

```scala
abstract class P[+A, +B] {
  def fst: A
  def snd: B
}
```

With this variance annotation, type instances of ´P´ subtype covariantly with respect to their arguments.
For instance,

```scala
P[IOException, String] <: P[Throwable, AnyRef]
```

If the members of ´P´ are mutable variables, the same variance annotation becomes illegal.

```scala
abstract class Q[+A, +B](x: A, y: B) {
  var fst: A = x           // **** error: illegal variance:
  var snd: B = y           // `A', `B' occur in invariant position.
}
```

If the mutable variables are object-private, the class definition becomes legal again:

```scala
abstract class R[+A, +B](x: A, y: B) {
  private var fst: A = x        // OK
  private var snd: B = y        // OK
}
```

###### Example

The following variance annotation is illegal, since ´A´ appears in contravariant position in the parameter of `append`:

```scala
abstract class Sequence[+A] {
  def append(x: Sequence[A]): Sequence[A]
                  // **** error: illegal variance:
                  // `A' occurs in contravariant position.
}
```

The problem can be avoided by generalizing the type of `append` by means of a lower bound:

```scala
abstract class Sequence[+A] {
  def append[B >: A](x: Sequence[B]): Sequence[B]
}
```

###### Example

```scala
abstract class OutputChannel[-A] {
  def write(x: A): Unit
}
```

With that annotation, we have that `OutputChannel[AnyRef]` conforms to `OutputChannel[String]`.
That is, a channel on which one can write any object can substitute for a channel on which one can write only strings.

## Method Definitions

```ebnf
Def                ::=  ‘def’ FunDef
FunDef             ::=  FunSig [‘:’ Type] [‘=’ Expr]
FunSig             ::=  id [FunTypeParamClause] ParamClauses
FunTypeParamClause ::=  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
ParamClauses       ::=  {ParamClause} [[nl] ‘(’ ‘implicit’ Params ‘)’]
ParamClause        ::=  [nl] ‘(’ [Params] ‘)’
Params             ::=  Param {‘,’ Param}
Param              ::=  {Annotation} id [‘:’ ParamType] [‘=’ Expr]
ParamType          ::=  Type
                     |  ‘=>’ Type
                     |  Type ‘*’
```

An _abstract method definition_ has the form `def ´f\,\mathit{psig}´: ´T´`, where ´f´ is the method's name, ´\mathit{psig}´ is its parameter signature and ´T´ is its result type.
A _concrete method definition_ `def ´f\,\mathit{psig}´: ´T´ = ´e´` also includes a _method body_ ´e´, i.e. an expression which defines the method's result.
A parameter signature consists of an optional type parameter clause `[´\mathit{tps}\,´]`, followed by zero or more value parameter clauses `(´\mathit{ps}_1´)...(´\mathit{ps}_n´)`.

If there is no type or term parameter clause, a method definition introduces a method with a proper type, which is also its result type.
Otherwise, it introduces a method with a methodic type whose parameter types and result type are as given.

The type of the method body is expected to [conform](06-expressions.html#expression-typing) to the method's declared result type, if one is given.
If the method definition is not recursive, the result type may be omitted, in which case it is determined from the packed type of the method body.

A _type parameter clause_ ´\mathit{tps}´ consists of one or more [type definitions](#type-definitions), which introduce type parameters, possibly with bounds.
The scope of a type parameter includes the whole signature, including any of the type parameter bounds as well as the method body, if it is present.

A _value parameter clause_ ´\mathit{ps}´ consists of zero or more formal parameter bindings such as `´x´: ´T´` or `´x: T = e´`, which bind value parameters and associate them with their types.

A unary operator must not have explicit parameter lists even if they are empty.
A unary operator is a method named `"unary_´op´"` where ´op´ is one of `+`, `-`, `!`, or `~`.

### Default Arguments

Each value parameter may optionally define a default argument.
The default argument expression ´e´ is type-checked with an expected type ´T'´ obtained by replacing all occurrences of the method's type parameters in ´T´ by the undefined type.

For every parameter ´p_{i,j}´ with a default argument, a method named `´f\$´default´\$´n` is generated which computes the default argument expression.
Here, ´n´ denotes the parameter's position in the method definition.
These methods are parametrized by the type parameter clause `[´\mathit{tps}\,´]` and all value parameter clauses `(´\mathit{ps}_1´)...(´\mathit{ps}_{i-1}´)` preceding ´p_{i,j}´.
The `´f\$´default´\$´n` methods are inaccessible for user programs.

###### Example
In the method

```scala
def compare[T](a: T = 0)(b: T = a) = (a == b)
```

the default expression `0` is type-checked with an undefined expected type.
When applying `compare()`, the default value `0` is inserted and `T` is instantiated to `Int`.
The methods computing the default arguments have the form:

```scala
def compare´\$´default´\$´1[T]: Int = 0
def compare´\$´default´\$´2[T](a: T): T = a
```

The scope of a formal value parameter name ´x´ comprises all subsequent parameter clauses, as well as the method return type and the method body, if they are given.
Both type parameter names and value parameter names must be pairwise distinct.

A default value which depends on earlier parameters uses the actual arguments if they are provided, not the default arguments.

```scala
def f(a: Int = 0)(b: Int = a + 1) = b // OK
// def f(a: Int = 0, b: Int = a + 1)  // "error: not found: value a"
f(10)()                               // returns 11 (not 1)
```

If an [implicit argument](07-implicits.html#implicit-parameters) is not found by implicit search, it may be supplied using a default argument.

```scala
implicit val i: Int = 2
def f(implicit x: Int, s: String = "hi") = s * x
f                                     // "hihi"
```

### By-Name Parameters

```ebnf
ParamType          ::=  ‘=>’ Type
```

The type of a value parameter may be prefixed by `=>`, e.g. `´x´: => ´T´`.
The type of such a parameter is then the [by-name type](./03-types.html#by-name-types) `=> ´T´`.
This indicates that the corresponding argument is not evaluated at the point of method application, but instead is evaluated at each use within the method.
That is, the argument is evaluated using _call-by-name_.

The by-name modifier is disallowed for parameters of classes that carry a `val` or `var` prefix, including parameters of case classes for which a `val` prefix is implicitly generated.

###### Example
The definition

```scala
def whileLoop (cond: => Boolean) (stat: => Unit): Unit
```

indicates that both parameters of `whileLoop` are evaluated using call-by-name.

### Repeated Parameters

```ebnf
ParamType          ::=  Type ‘*’
```

The last value parameter of a parameter section may be suffixed by `'*'`, e.g. `(..., ´x´:´T´*)`.
The type of such a _repeated_ parameter inside the method is then the sequence type `scala.Seq[´T´]`.
Methods with repeated parameters `´T´*` take a variable number of arguments of type ´T´.
That is, if a method ´m´ with type `(´p_1:T_1, ..., p_n:T_n, p_s:S´*)´U´` is applied to arguments ´(e_1, ..., e_k)´ where ´k \geq n´, then ´m´ is taken in that application to have type ´(p_1:T_1, ..., p_n:T_n, p_s:S, ..., p_{s'}:S)U´, with ´k - n´ occurrences of type ´S´ where any parameter names beyond ´p_s´ are fresh. The only exception to this rule is if the last argument is marked to be a _sequence argument_ via a `_*` type annotation.
If ´m´ above is applied to arguments `(´e_1, ..., e_n, e'´: _*)`, then the type of ´m´ in that application is taken to be `(´p_1:T_1, ... , p_n:T_n,p_{s}:´scala.Seq[´S´])`.

It is not allowed to define any default arguments in a parameter section with a repeated parameter.

###### Example
The following method definition computes the sum of the squares of a variable number of integer arguments.

```scala
def sum(args: Int*) = {
  var result = 0
  for (arg <- args) result += arg
  result
}
```

The following applications of this method yield `0`, `1`, `6`, in that order.

```scala
sum()
sum(1)
sum(1, 2, 3)
```

Furthermore, assume the definition:

```scala
val xs = List(1, 2, 3)
```

The following application of method `sum` is ill-formed:

```scala
sum(xs)       // ***** error: expected: Int, found: List[Int]
```

By contrast, the following application is well formed and yields again the result `6`:

```scala
sum(xs: _*)
```

### Method Return Type Inference

A class member definition ´m´ that overrides some other method ´m'´ in a base class of ´C´ may leave out the return type, even if it is recursive.
In this case, whether or not `m` is recursive, its return type will be the return type of ´m'´.

###### Example
Assume the following definitions:

```scala
trait I {
  def factorial(x: Int): Int
}
class C extends I {
  def factorial(x: Int) = if (x == 0) 1 else x * factorial(x - 1)
}
```

Here, it is OK to leave out the result type of `factorial` in `C`, even though the method is recursive.

### Tail-Recursive Call Elimination

Method definitions which contain self-recursive invocations in tail position are optimized for stack safety.
Self-invocations which are the last operation before returning from the method are replaced with jumps to the beginning of the method, much as in a while loop.
Sibling-invocations, in which a method calls itself but with a different instance as receiver, are also optimized.

This transform is performed automatically by the compiler whenever possible.
A method definition bearing the annotation, `scala.annotation.tailrec`, will fail to compile if the transform is not possible.
(The annotation is intended for cases where deoptimization would likely result in a stack overflow.)

```scala
@annotation.tailrec
def sum(xs: List[Int], acc: Int): Int =
  xs match { case h :: t => sum(t, acc + h) case _ => acc }
```

<!-- ## Overloaded Definitions
\label{sec:overloaded-defs}
\todo{change}

An overloaded definition is a set of ´n > 1´ value or method
definitions in the same statement sequence that define the same name,
binding it to types `´T_1 \commadots T_n´`, respectively.
The individual definitions are called _alternatives_.  Overloaded
definitions may only appear in the statement sequence of a template.
Alternatives always need to specify the type of the defined entity
completely.  It is an error if the types of two alternatives ´T_i´ and
´T_j´ have the same erasure (\sref{sec:erasure}).

\todo{Say something about bridge methods.}
%This must be a well-formed
%overloaded type -->

## Import Clauses

```
Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
ImportExpr        ::= SimpleRef {‘.’ id} ‘.’ ImportSpecifier
                    | SimpleRef `as` id
ImportSpecifier   ::=  NamedSelector
                    |  WildcardSelector
                    | ‘{’ ImportSelectors ‘}’
NamedSelector     ::=  id [(‘as’ | ’=>’) (id | ‘_’)]
WildcardSelector  ::=  ‘*’ | ’_’ | ‘given’ [InfixType]
ImportSelectors   ::=  NamedSelector [‘,’ ImportSelectors]
                    |  WildCardSelector {‘,’ WildcardSelector}
```

- In a `NamedSelector`, `=>` can only be used when inside an `ImportSelectors` and is then equivalent to `as`, to be deprecated in the future.
- In a `WildcardSelector`, `_` is equivalent to `*`, to be deprecated in the future.

An `ImportSpecifier` that is a single `NamedSelector` or `WildcardSelector` is equivalent to an `‘{‘ ImportSelectors ‘}‘` list with that single selector.

An import clause with multiple import expressions `import ´p_1´.´I_1, ..., p_n´.´I_n´` is interpreted as a sequence of import clauses `import ´p_1´.´I_1´; ...; import ´p_n´.´I_n´`.

An import clause with a single import expression has the form `import ´p´.´I´` where ´p´ is a [prefix](03-types.html#designator-types) and ´I´ is an import specifier.
The import specifier determines a set of names of importable members of ´p´ which are made available without qualification as well as a set of importable `given` members which are made available in the implicit scope.
A member ´m´ of ´p´ is _importable_ if it is [accessible](05-classes-and-objects.html#modifiers).
The most general form of an import specifier is a list of _import selectors_

```scala
{ ´x_1´ as ´y_1, ..., x_n´ as ´y_n´, *, given ´T_1´, ..., given ´T_m´, given }
```

for ´n \geq 0´ and ´m \geq 0´, where the wildcards `‘*’` and `’given’` may be absent.
They are decomposed into non-given selectors and given selectors.

### Non-given Imports

Non-given selectors make available each importable member `´p´.´x_i´` under the unqualified name ´y_i´.
In other words, every import selector `´x_i´ as ´y_i´` renames `´p´.´x_i´` to ´y_i´.
When `as ´y_i´` is omitted, ´y_i´ is assumed to be ´x_i´.
If a final wildcard `‘*’` is present, all non-`given` importable members ´z´ of ´p´ other than `´x_1, ..., x_n, y_1, ..., y_n´` are also made available under their own unqualified names.

Non-given import selectors work in the same way for type and term members.
For instance, an import clause `import ´p´.´x´ as ´y´` renames the term name `´p´.´x´` to the term name ´y´ and the type name `´p´.´x´` to the type name ´y´.
At least one of these two names must reference an importable member of ´p´.

If the target in an import selector is an underscore `as _`, the import selector hides access to the source member instead of importing it.
For instance, the import selector `´x´ as _` “renames” ´x´ to the underscore symbol (which is not accessible as a name in user programs), and thereby effectively prevents unqualified access to ´x´.
This is useful if there is a final wildcard in the same import selector list, which imports all members not mentioned in previous import selectors.

The scope of a binding introduced by a non-given import clause starts immediately after the import clause and extends to the end of the enclosing block, template, package clause, or compilation unit, whichever comes first.

### Given Imports

Given selectors make available in the implicit scope all the importable `given` and `implicit` members `´p´.´x´` such that `´p.x´` is a subtype of ´T_i´.
A bare `given` selector without type is equivalent to `given scala.Any`.

The names of the given members are irrelevant for the selection, and are not made available in the normal scope of unqualified names.

###### Example
Consider the object definition:

```scala
object M {
  def z = 0
  def one = 1
  def add(x: Int, y: Int): Int = x + y
}
```

Then the block

```scala
{
  import M.{one, z as zero, *}
  add(zero, one)
}
```

is equivalent to the block

```scala
{
  M.add(M.z, M.one)
}
```
