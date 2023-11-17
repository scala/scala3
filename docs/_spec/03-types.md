---
title: Types
layout: default
chapter: 3
---

# Types

```ebnf
  Type              ::=  FunctionArgTypes ‘=>’ Type
                      |  TypeLambdaParams ‘=>>’ Type
                      |  InfixType
  FunctionArgTypes  ::=  InfixType
                      |  ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
  TypeLambdaParams  ::=  ‘[’ TypeLambdaParam {‘,’ TypeLambdaParam} ‘]’
  TypeLambdaParam   ::=  {Annotation} (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
  InfixType         ::=  CompoundType {id [nl] CompoundType}
  CompoundType      ::=  AnnotType {‘with’ AnnotType} [Refinement]
                      |  Refinement
  AnnotType         ::=  SimpleType {Annotation}
  SimpleType        ::=  SimpleType TypeArgs
                      |  SimpleType ‘#’ id
                      |  StableId
                      |  Path ‘.’ ‘type’
                      |  Literal
                      |  ‘(’ Types ‘)’
  TypeArgs          ::=  ‘[’ Types ‘]’
  Types             ::=  Type {‘,’ Type}
```

We distinguish between proper types and type constructors, which take type parameters and yield types.
All types have a _kind_, either the kind of proper types or a _higher kind_.
A subset of proper types called _value types_ represents sets of (first-class) values.
Types are either _concrete_ or _abstract_.

Every concrete value type can be represented as a _class type_, i.e. a [type designator](#type-designators) that refers to a [class or a trait](05-classes-and-objects.html#class-definitions) [^1], or as a [compound type](#compound-types) representing an intersection of types, possibly with a [refinement](#compound-types) that further constrains the types of its members.

<!--
A shorthand exists for denoting [function types](#function-types)
-->
Abstract types are introduced by [type parameters](04-basic-declarations-and-definitions.html#type-parameters) and [abstract type bindings](04-basic-declarations-and-definitions.html#type-declarations-and-type-aliases).
Parentheses in types can be used for grouping.

[^1]: We assume that objects and packages also implicitly
      define a class (of the same name as the object or package, but
      inaccessible to user programs).

Non-value types capture properties of identifiers that [are not values](#non-value-types).
For example, a [type constructor](#type-constructors) does not directly specify a type of values.
However, when a type constructor is applied to the correct type arguments, it yields a proper type, which may be a value type.

Non-value types are expressed indirectly in Scala.
E.g., a method type is described by writing down a method signature, which in itself is not a real type, although it  gives rise to a corresponding [method type](#method-types).
Type constructors are another example, as one can write `type Swap[m[_, _], a,b] = m[b, a]`, but there is no syntax to write the corresponding anonymous type function directly.

`AnyKind` is the super type of all types in the Scala type system.
It has all possible kinds to encode [kind polymorphism](#kind-polymorphism).
As such, it is neither a value type nor a type constructor.

## Paths

```ebnf
Path            ::=  StableId
                  |  [id ‘.’] this
StableId        ::=  id
                  |  Path ‘.’ id
                  |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
ClassQualifier  ::= ‘[’ id ‘]’
```
<!-- TODO: Clarify paragraph -->
Paths are not types themselves, but they can be a part of named types and in that function form a central role in Scala's type system.

A path is one of the following.

- The empty path ε (which cannot be written explicitly in user programs).
- ´C.´`this`, where ´C´ references a class.
  The path `this` is taken as a shorthand for ´C.´`this` where ´C´ is the name of the class directly enclosing the reference.
- ´p.x´ where ´p´ is a path and ´x´ is a stable member of ´p´.
  _Stable members_ are packages or members introduced by object definitions or by value definitions of [non-volatile types](#volatile-types).
- ´C.´`super`´.x´ or ´C.´`super`´[M].x´
  where ´C´ references a class and ´x´ references a stable member of the super class or designated parent class ´M´ of ´C´.
  The prefix `super` is taken as a shorthand for ´C.´`super` where ´C´ is the name of the class directly enclosing the reference.

A _stable identifier_ is a path which ends in an identifier.

## Value Types

Every value in Scala has a type which is of one of the following forms.

### Singleton Types

```ebnf
SimpleType  ::=  Path ‘.’ ‘type’
```

A _singleton type_ is of the form ´p.´`type`.
Where ´p´ is a path pointing to a value which [conforms](06-expressions.html#expression-typing) to `scala.AnyRef`, the type denotes the set of values consisting of `null` and the value denoted by ´p´ (i.e., the value ´v´ for which `v eq p`).
Where the path does not conform to `scala.AnyRef` the type denotes the set consisting of only the value denoted by ´p´.

<!-- a pattern match/type test against a singleton type `p.type` desugars to `_ eq p` -->

### Literal Types

```ebnf
SimpleType  ::=  Literal
```

A literal type `lit` is a special kind of singleton type which denotes the single literal value `lit`.
Thus, the type ascription `1: 1` gives the most precise type to the literal value `1`:  the literal type `1`.

At run time, an expression `e` is considered to have literal type `lit` if `e == lit`.
Concretely, the result of `e.isInstanceOf[lit]` and `e match { case _ : lit => }` is determined by evaluating `e == lit`.

Literal types are available for all types for which there is dedicated syntax except `Unit`.
This includes the numeric types (other than `Byte` and `Short` which don't currently have syntax), `Boolean`, `Char` and `String`.

### Stable Types
A _stable type_ is a singleton type, a literal type, or a type that is declared to be a subtype of trait `scala.Singleton`.

### Type Projection

```ebnf
SimpleType  ::=  SimpleType ‘#’ id
```

A _type projection_ ´T´#´x´ references the type member named ´x´ of type ´T´.

<!--
The following is no longer necessary:
If ´x´ references an abstract type member, then ´T´ must be a
[stable type](#singleton-types)
-->

### Type Designators

```ebnf
SimpleType  ::=  StableId
```

A _type designator_ refers to a named value type.
It can be simple or qualified.
All such type designators are shorthands for type projections.

Specifically, the unqualified type name ´t´ where ´t´ is bound in some class, object, or package ´C´ is taken as a shorthand for
´C.´`this.type#`´t´.
If ´t´ is not bound in a class, object, or package, then ´t´ is taken as a shorthand for ε`.type#`´t´.

A qualified type designator has the form `p.t` where `p` is a [path](#paths) and _t_ is a type name.
Such a type designator is equivalent to the type projection `p.type#t`.

###### Example

Some type designators and their expansions are listed below.
We assume a local type parameter ´t´, a value `maintable` with a type member `Node` and the standard class `scala.Int`,

| Designator          | Expansion                 |
|-------------------- | --------------------------|
|t                    | ε.type#t                  |
|Int                  | scala.type#Int            |
|scala.Int            | scala.type#Int            |
|data.maintable.Node  | data.maintable.type#Node  |

### Parameterized Types

```ebnf
SimpleType      ::=  SimpleType TypeArgs
TypeArgs        ::=  ‘[’ Types ‘]’
```

A _parameterized type_ ´T[ T_1, ..., T_n ]´ consists of a type designator ´T´ and type arguments ´T_1, ..., T_n´ where ´n \geq 1´.
´T´ must refer to a type constructor which takes ´n´ type parameters ´a_1, ..., a_n´.

<!-- TODO Mention well-kinded conformance when we introduce kind-polymorphism -->
Say the type parameters have lower bounds ´L_1, ..., L_n´ and upper bounds ´U_1, ..., U_n´.
The parameterized type is well-formed if each type argument _conforms to its bounds_, i.e. ´\sigma L_i <: T_i <: \sigma U_i´ where ´\sigma´ is the substitution ´[ a_1 := T_1, ..., a_n := T_n ]´.

#### Example Parameterized Types

Given the partial type definitions:

```scala
class TreeMap[A <: Comparable[A], B] { ... }
class List[A] { ... }
class I extends Comparable[I] { ... }

class F[M[A], X] { ... }
class S[K <: String] { ... }
class G[M[Z <: I], I] { ... }
```

the following parameterized types are well-formed:

```scala
TreeMap[I, String]
List[I]
List[List[Boolean]]

F[List, Int]
G[S, String]
```

and the following types are ill-formed:

```scala
TreeMap[I]            // illegal: wrong number of parameters
TreeMap[List[I], Int] // illegal: type parameter not within bound

F[Int, Boolean]       // illegal: Int is not a type constructor
F[TreeMap, Int]       // illegal: TreeMap takes two parameters,
                      //   F expects a constructor taking one
G[S, Int]             // illegal: S constrains its parameter to
                      //   conform to String,
                      // G expects type constructor with a parameter
                      //   that conforms to Int
```

#### Wildcard Type Argument

<!-- TODO Update the syntax to use '?' -->
```ebnf
WildcardType   ::=  ‘_’ TypeBounds
```

A _wildcard type argument_ is of the form `_´\;´>:´\,L\,´<:´\,U´`.
A wildcard type must appear as a type argument of a parameterized type.
The parameterized type to which the wildcard type is applied cannot be an abstract type constructor.

Both bound clauses may be omitted.
If both bounds are omitted, the real bounds are inferred from the bounds of the corresponding type parameter in the target type constructor.
Otherwise, if a lower bound clause `>:´\,L´` is missing, `>:´\,´scala.Nothing` is assumed.
Otherwise, if an upper bound clause `<:´\,U´` is missing, `<:´\,´scala.Any` is assumed.

Given the [above type definitions](#example-parameterized-types), the following types are well-formed:

```scala
List[_] // inferred as List[_ >: Nothing <: Any]
List[_ <: java.lang.Number]
S[_ <: String]
F[_, Boolean]
```

and the following code contains an ill-formed type:

```scala
trait H[F[A]]:
  def f: F[_] // illegal : an abstract type constructor
              // cannot be applied to wildcard arguments.
```

Wildcard types may also appear as parts of [infix types](#infix-types), [function types](#function-types), or [tuple types](#tuple-types).
Their expansion is then the expansion in the equivalent parameterized type.

##### Simplification Rules

Let ´T[T_1, ..., T_n]´ be a parameterized type.
Then, applying a wildcard type argument ´t´ of the form ´\\_ >: L <: U´ at the ´i´'th position obeys the following equivalences:

- If the type parameter ´T_i´ is declared covariant, then ´t \equiv U´
- If the type parameter ´T_i´ is declared contravariant, then ´t \equiv L´

### Tuple Types

```ebnf
SimpleType    ::=   ‘(’ Types ‘)’
```

A _tuple type_ ´(T_1, ..., T_n)´ where ´n \geq 2´ is an alias for the type `´T_1´ *: ... *: ´T_n´ *: scala.EmptyTuple`.

Notes:
- `(´T´)` is just the type ´T´, and not `´T´ *: scala.EmptyTuple`.
- `()` is not a valid type, and not `scala.EmptyTuple`.

If ´n \leq 22´, the type `´T_1´ *: ... *: ´T_n´ *: scala.EmptyTuple` is both a subtype and a supertype of tuple class `scala.Tuple´_n´[´T_1´, ..., ´T_n´]`.

Tuple classes are case classes whose fields can be accessed using selectors `_1`, ..., `_n`.
Their functionality is abstracted in the corresponding `scala.Product_´n´` trait.
The _n_-ary tuple class and product trait are defined at least as follows in the standard Scala library (they might also add other methods and implement other traits).

```scala
case class Tuple´_n´[+´T_1´, ..., +´T_n´](_1: ´T_1´, ..., _n: ´T_n´)
extends Product´_n´[´T_1´, ..., ´T_n´]

trait Product´_n´[+´T_1´, ..., +´T_n´] extends Product:
  override def productArity = ´n´
  def _1: ´T_1´
  ...
  def _n: ´T_n´
```

### Annotated Types

```ebnf
AnnotType  ::=  SimpleType {Annotation}
```

An _annotated type_ ´T´ ´a_1, ..., a_n´ attaches [annotations](11-annotations.html#user-defined-annotations) ´a_1, ..., a_n´ to the type ´T´.

###### Example

The following type adds the `@suspendable` annotation to the type `String`:

```scala
String @suspendable
```

### Compound Types

```ebnf
CompoundType    ::=  AnnotType {‘with’ AnnotType} [Refinement]
                  |  Refinement
Refinement      ::=  [nl] ‘{’ RefineStat {semi RefineStat} ‘}’
RefineStat      ::=  Dcl
                  |  ‘type’ TypeDef
                  |
```

A _compound type_ ´T_1´ `with` ... `with` ´T_n \\{ R \\}´ represents objects with members as given in the component types ´T_1, ..., T_n´ and the refinement ´\\{ R \\}´.
A refinement ´\\{ R \\}´ contains declarations and type definitions.
If a declaration or definition overrides a declaration or definition in one of the component types ´T_1, ..., T_n´, the usual rules for [overriding](05-classes-and-objects.html#overriding) apply; otherwise the declaration or definition is said to be “structural” [^2].

[^2]: A reference to a structurally defined member (method call or access to a value or variable) may generate binary code that is significantly slower than an equivalent code to a non-structural member.

Within a method declaration in a structural refinement, the type of any value parameter may only refer to type parameters or abstract types that are contained inside the refinement.
That is, it must refer either to a type parameter of the method itself, or to a type definition within the refinement.
This restriction does not apply to the method's result type.

If no refinement is given, the empty refinement is implicitly added, i.e. ´T_1´ `with` ... `with` ´T_n´ is a shorthand for ´T_1´ `with` ... `with` ´T_n \\{\\}´.

A compound type may also consist of just a refinement ´\\{ R \\}´ with no preceding component types.
Such a type is equivalent to `AnyRef` ´\\{ R \\}´.

###### Example

The following example shows how to declare and use a method which has a parameter type that contains a refinement with structural declarations.

```scala
case class Bird (val name: String) extends Object {
        def fly(height: Int) = ...
...
}
case class Plane (val callsign: String) extends Object {
        def fly(height: Int) = ...
...
}
def takeoff(
            runway: Int,
      r: { val callsign: String; def fly(height: Int) }) = {
  tower.print(r.callsign + " requests take-off on runway " + runway)
  tower.read(r.callsign + " is clear for take-off")
  r.fly(1000)
}
val bird = new Bird("Polly the parrot"){ val callsign = name }
val a380 = new Plane("TZ-987")
takeoff(42, bird)
takeoff(89, a380)
```

Although `Bird` and `Plane` do not share any parent class other than `Object`, the parameter _r_ of method `takeoff` is defined using a refinement with structural declarations to accept any object that declares a value `callsign` and a `fly` method.

### Infix Types

```ebnf
InfixType     ::=  CompoundType {id [nl] CompoundType}
```

An _infix type_ ´T_1´ `op` ´T_2´ consists of an infix operator `op` which gets applied to two type operands ´T_1´ and ´T_2´.
The type is equivalent to the type application `op`´[T_1, T_2]´.
The infix operator `op` may be an arbitrary identifier.

Type operators follow the same [precedence and associativity as term operators](06-expressions.html#prefix-infix-and-postfix-operations).
For example, `A + B * C` parses as `A + (B * C)` and `A | B & C` parses as `A | (B & C)`.
Type operators ending in a colon ‘:’ are right-associative; all other operators are left-associative.

In a sequence of consecutive type infix operations ´t_0 \, \mathit{op} \, t_1 \, \mathit{op_2} \, ... \, \mathit{op_n} \, t_n´, all operators ´\mathit{op}\_1, ..., \mathit{op}\_n´ must have the same associativity.
If they are all left-associative, the sequence is interpreted as ´(... (t_0 \mathit{op_1} t_1) \mathit{op_2} ...) \mathit{op_n} t_n´, otherwise it is interpreted as ´t_0 \mathit{op_1} (t_1 \mathit{op_2} ( ... \mathit{op_n} t_n) ...)´.

The type operators `|` and `&` are not really special.
Nevertheless, unless shadowed, they resolve to `scala.|` and `scala.&`, which represent [union and intersection types](#union-and-intersection-types), respectively.

### Function Types

```ebnf
Type              ::=  FunTypeArgs ‘=>’ Type
FunTypeArgs       ::=  InfixType
                    |  ‘(’ [ FunArgTypes ] ‘)’
                    |  FunParamClause
FunArgTypes       ::=  FunArgType { ‘,’ FunArgType }
FunArgType        ::=  Type
                    |  ‘=>’ Type
FunParamClause    ::=  ‘(’ TypedFunParam {‘,’ TypedFunParam } ‘)’
TypedFunParam     ::=  id ‘:’ Type
```

The concrete function type ´(T_1, ..., T_n) \Rightarrow R´ represents the set of function values that take arguments of types ´T_1, ..., Tn´ and yield results of type ´R´.
The case of exactly one argument type ´T \Rightarrow R´ is a shorthand for ´(T) \Rightarrow R´.
An argument type of the form ´\Rightarrow T´ represents a [call-by-name parameter](04-basic-declarations-and-definitions.html#by-name-parameters) of type ´T´.

Function types associate to the right, e.g. ´S \Rightarrow T \Rightarrow R´ is the same as ´S \Rightarrow (T \Rightarrow R)´.

Function types are [covariant](04-basic-declarations-and-definitions.md#variance-annotations) in their result type and [contravariant](04-basic-declarations-and-definitions.md#variance-annotations) in their argument types.

Function types translate into internal class types that define an `apply` method.
Specifically, the ´n´-ary function type ´(T_1, ..., T_n) \Rightarrow R´ translates to the internal class type `scala.Function´_n´[´T_1´, ..., ´T_n´, ´R´]`.
In particular ´() \Rightarrow R´ is a shorthand for class type `scala.Function´_0´[´R´]`.

Such class types behave as if they were instances of the following trait:

```scala
trait Function´_n´[-´T_1´, ..., -´T_n´, +´R´]:
  def apply(´x_1´: ´T_1´, ..., ´x_n´: ´T_n´): ´R´
```

Their exact supertype and implementation can be consulted in the [function classes section](./12-the-scala-standard-library.md#the-function-classes) of the standard library page in this document.

_Dependent function types_ are function types whose parameters are named and can referred to in result types.
In the concrete type ´(x_1: T_1, ..., x_n: T_n) \Rightarrow R´, ´R´ can refer to the parameters ´x_i´, notably to form path-dependent types.
It translates to the internal [refined type](#refined-types)
```scala
scala.Function´_n´[´T_1´, ..., ´T_n´, ´S´] {
  def apply(´x_1´: ´T_1´, ..., ´x_n´: ´T_n´): ´R´
}
```
where ´S´ is the least super type of ´R´ that does not mention any of the ´x_i´.

_Polymorphic function types_ are function types that take type arguments.
Their result type must be a function type.
In the concrete type ´[a_1 >: L_1 <: H_1, ..., a_n >: L_1 <: H_1] => (T_1, ..., T_m) => R´, the types ´T_j´ and ´R´ can refer to the type parameters ´a_i´.
It translates to the internal refined type
```scala
scala.PolyFunction {
  def apply[´a_1 >: L_1 <: H_1, ..., a_n >: L_1 <: H_1´](´x_1´: ´T_1´, ..., ´x_n´: ´T_n´): ´R´
}
```

### Concrete Refined Types

```ebnf
RefinedType           ::=  AnnotType {[nl] Refinement}
SimpleType1           ::=  ...
                        |  Refinement
Refinement            ::=  :<<< [RefineDcl] {semi [RefineDcl]} >>>

RefineDcl             ::=  ‘val’ ValDcl
                        |  ‘def’ DefDcl
                        |  ‘type’ {nl} TypeDcl
```

In the concrete syntax of types, refinements can contain several refined declarations.
Moreover, the refined declarations can refer to each other as well as to members of the parent type, i.e., they have access to `this`.

In the internal types, each refinement defines exactly one refined declaration, and references to `this` must be made explicit in a recursive type.

The conversion from the concrete syntax to the abstract syntax works as follows:

1. Create a fresh recursive this name ´\alpha´.
2. Replace every implicit or explicit reference to `this` in the refinement declarations by ´\alpha´.
3. Create nested [refined types](#refined-types), one for every refined declaration.
4. Unless ´\alpha´ was never actually used, wrap the result in a [recursive type](#recursive-types) `{ ´\alpha´ => ´...´ }`.

### Concrete Type Lambdas

```ebnf
TypeLambda            ::= TypeLambdaParams ‘=>>’ Type
TypeLambdaParams      ::=  ‘[’ TypeLambdaParam {‘,’ TypeLambdaParam} ‘]’
TypeLambdaParam       ::=  {Annotation} (id | ‘_’) [TypeParamClause] TypeBounds
TypeParamClause       ::=  ‘[’ VariantTypeParam {‘,’ VariantTypeParam} ‘]’
VariantTypeParam      ::=  {Annotation} [‘+’ | ‘-’] (id | ‘_’) [TypeParamClause] TypeBounds
```

At the top level of concrete type lambda parameters, variance annotations are not allowed.
However, in internal types, all type lambda parameters have explicit variance annotations.

When translating a concrete type lambda into an internal one, the variance of each type parameter is _inferred_ from its usages in the body of the type lambda.

## Definitions

From here onwards, we refer to internal types by default.

### Kinds

The Scala type system is fundamentally higher-kinded.
_Types_ are either _proper types_, _type constructors_ or _poly-kinded types_.

- Proper types are the types of _terms_.
- Type constructors are type-level functions from types to types.
- Poly-kinded types can take various kinds.

All types live in a single lattice with respect to a [_conformance_](#conformance) relationship ´<:´.
The _top type_ is `AnyKind` and the _bottom type_ is `Nothing`: all types conform to `AnyKind`, and `Nothing` conforms to all types.
They can be referred to as the standard library entities `scala.AnyKind` and `scala.Nothing`, respectively.

Types can be _concrete_ or _abstract_.
An abstract type ´T´ always has lower and upper bounds ´L´ and ´H´ such that ´L >: T´ and ´T <: H´.
A concrete type ´T´ is considered to have itself as both lower and upper bound.

The kind of a type is indicated by its (transitive) upper bound:

- A type `´T <:´ scala.Any` is a proper type.
- A type `´T <: K´` where ´K´ is a [_type lambda_](#type-lambdas) (of the form `[´\pm a_1 >: L_1 <: H_1´, ..., ´\pm a_n >: L_n <: H_n´] =>> ´U´`) is a type constructor.
- Other types are poly-kinded; they are neither proper types nor type constructors.

As a consequece, `AnyKind` itself is poly-kinded.
`Nothing` is _universally-kinded_: it has all kinds at the same time, since it conforms to all types.

With this representation, it is rarely necessary to explicitly talk about the kinds of types.
Usually, the kinds of types are implicit through their bounds.

Another way to look at it is that type bounds _are_ kinds.
They represent sets of types: ´>: L <: H´ denotes the set of types ´T´ such that ´L <: T´ and ´T <: H´.
A set of types can be seen as a _type of types_, i.e., as a _kind_.

#### Conventions

Type bounds are formally always of the form `´>: L <: H´`.
By convention, we can omit either of both bounds in writing.

- When omitted, the lower bound ´L´ is `Nothing`.
- When omitted, the higher bound ´H´ is `Any` (_not_ `AnyKind`).

These conventions correspond to the defaults in the concrete syntax.

### Proper Types

Proper types are also called _value types_, as they represent sets of _values_.

_Stable types_ are value types that contain exactly one non-`null` value.
Stable types can be used as prefixes in named [designator types](#designator-types).
The stable types are

- designator types referencing a stable term,
- this types,
- super types,
- literal types,
- recursive this types, and
- skolem types.

Every stable type ´T´ is concrete and has an _underlying_ type ´U´ such that ´T <: U´.

### Type Constructors

To each type constructor corresponds an _inferred type parameter clause_ which is computed as follows:

- For a [type lambda](#type-lambdas), its type parameter clause (including variance annotations).
- For a [polymorphic class type](#type-designators), the type parameter clause of the referenced class definition.
- For a non-class [type designator](#type-designators), the inferred clause of its upper bound.

### Type Definitions

A _type definition_ ´D´ represents the right-hand-side of a `type` declaration or the bounds of a type parameter.
It is either:

- a type alias of the form ´= U´, or
- an abstract type definition with bounds ´>: L <: H´.

All type definitions have a lower bound ´L´ and an upper bound ´H´, which are types.
For type aliases, ´L = H = U´.

The type definition of a type parameter is never a type alias.

## Types

### Type Lambdas

```ebnf
TypeLambda     ::=  ‘[‘ TypeParams ‘]‘ ‘=>>‘ Type
TypeParams     ::=  TypeParam {‘,‘ TypeParam}
TypeParam      ::=  ParamVariance id TypeBounds
ParamVariance  ::=  ε | ‘+‘ | ‘-‘
```

A _type lambda_ of the form `[´\pm a_1 >: L_1 <: H_1´, ..., ´\pm a_n >: L_n <: H_n´] =>> ´U´` is a direct representation of a type constructor with ´n´ type parameters.
When applied to ´n´ type arguments that conform to the specified bounds, it produces another type ´U´.
Type lambdas are always concrete types.

The scope of a type parameter extends over the result type ´U´ as well as the bounds of the type parameters themselves.

All type constructors conform to some type lambda.

The type bounds of the parameters of a type lambda are in contravariant position, while its result type is in covariant position.
If some type constructor `´T <:´ [´\pm a_1 >: L_1 <: H_1´, ..., ´\pm a_n >: L_n <: H_n´] =>> ´U´`, then ´T´'s ´i´th type parameter bounds contain the bounds ´>: L_i <: H_i´, and its result type conforms to ´U´.

Note: the concrete syntax of type lambdas does not allow to specify variances for type parameters.
Instead, variances are inferred from the body of the lambda to be as general as possible.

##### Example

```scala
type Lst = [T] =>> List[T] // T is inferred to be covariant with bounds >: Nothing <: Any
type Fn = [A <: Seq[?], B] =>> (A => B) // A is inferred to be contravariant, B covariant

val x: Lst[Int] = List(1) // ok, Lst[Int] expands to List[Int]
val f: Fn[List[Int], Int] = (x: List[Int]) => x.head // ok

val g: Fn[Int, Int] = (x: Int) => x // error: Int does not conform to the bound Seq[?]

def liftPair[F <: [T] =>> Any](f: F[Int]): Any = f
liftPair[Lst](List(1)) // ok, Lst <: ([T] =>> Any)
```

### Designator Types

```ebnf
DesignatorType    ::=  Prefix ‘.‘ id
Prefix            ::=  Type
                    |  PackageRef
                    |  ε
PackageRef        ::=  id {‘.‘ id}
```

A designator type (or designator for short) is a reference to a definition.
Term designators refer to term definitions, while type designators refer to type definitions.

In the abstract syntax, the `id` retains whether it is a term or type.
In the concrete syntax, an `id` refers to a *type* designator, while `id.type` refers to a *term* designator.
In that context, term designators are often called _singleton types_.

Designators with an empty prefix ´\epsilon´ are called direct designators.
They refer to local definitions available in the scope:

- Local `type`, `object`, `val`, `lazy val`, `var` or `def` definitions
- Term or type parameters

The `id`s of direct designators are protected from accidental shadowing in the abstract syntax.
They retain the identity of the exact definition they refer to, rather than relying on scope-based name resolution. [^debruijnoralpha]

[^debruijnoralpha]: In the literature, this is often achieved through De Bruijn indices or through alpha-renaming when needed. In a concrete implementation, this is often achieved through retaining *symbolic* references in a symbol table.

The ´\epsilon´ prefix cannot be written in the concrete syntax.
A bare `id` is used instead and resolved based on scopes.

Named designators refer to *member* definitions of a non-empty prefix:

- Top-level definitions, including top-level classes, have a package ref prefix
- Class member definitions and refinements have a type prefix

#### Term Designators

A term designator ´p.x´ referring to a term definition `t` has an _underlying type_ ´U´.
If ´p = \epsilon´ or ´p´ is a package ref, the underlying type ´U´ is the _declared type_ of `t` and ´p.x´ is a stable type if an only if `t` is a `val` or `object` definition.
Otherwise, the underlying type ´U´ and whether ´p.x´ is a stable type are determined by [`memberType`](#member-type)`(´p´, ´x´)`.

All term designators are concrete types.
If `scala.Null ´<: U´`, the term designator denotes the set of values consisting of `null` and the value denoted by ´t´, i.e., the value ´v´ for which `t eq v`.
Otherwise, the designator denotes the singleton set only containing ´v´.

#### Type Designators

A type designator ´p.C´ referring to a _class_ definition (including traits and hidden object classes) is a _class type_.
If the class is monomorphic, the type designator is a value type denoting the set of instances of ´C´ or any of its subclasses.
Otherwise it is a type constructor with the same type parameters as the class definition.
All class types are concrete, non-stable types.

If a type designator ´p.T´ is not a class type, it refers to a type definition `T` (a type parameter or a `type` declaration) and has an _underlying [type definition](#type-definitions)_.
If ´p = \epsilon´ or ´p´ is a package ref, the underlying type definition is the _declared type definition_ of `T`.
Otherwise, it is determined by [`memberType`](#member-type)`(´p´, ´T´)`.
A non-class type designator is concrete (resp. stable) if and only if its underlying type definition is an alias ´U´ and ´U´ is itself concrete (resp. stable).

### Parameterized Types

```ebnf
ParameterizedType ::=  Type ‘[‘ TypeArgs ‘]‘
TypeArgs          ::=  TypeArg {‘,‘ TypeArg}
TypeArg           ::=  Type
                    |  WilcardTypeArg
WildcardTypeArg   ::=  ‘?‘ TypeBounds
```

A _parameterized type_ ´T[T_1, ..., T_n]´ consists of a type constructor ´T´ and type arguments ´T_1, ..., T_n´ where ´n \geq 1´.
The parameterized type is well-formed if

- ´T´ is a type constructor which takes ´n´ type parameters ´a_1, ..., a_n´, i.e., it must conform to a type lambda of the form ´[\pm a_1 >: L_1 <: H_1, ..., \pm a_n >: L_n <: H_n] => U´, and
- if ´T´ is an abstract type constructor, none of the type arguments is a wildcard type argument, and
- each type argument _conforms to its bounds_, i.e., given ´\sigma´ the substitution ´[a_1 := T_1, ..., a_n := T_n]´, for each type ´i´:
  - if ´T_i´ is a type and ´\sigma L_i <: T_i <: \sigma H_i´, or
  - ´T_i´ is a wildcard type argument ´? >: L_{Ti} <: H_{Ti}´ and ´\sigma L_i <: L_{Ti}´ and ´H_{Ti} <: \sigma H_i´.

´T[T_1, ..., T_n]´ is a _parameterized class type_ if and only if ´T´ is a [class type](#type-designators).
All parameterized class types are value types.

In the concrete syntax of wildcard type arguments, if both bounds are omitted, the real bounds are inferred from the bounds of the corresponding type parameter in the target type constructor (which must be concrete).
If only one bound is omitted, `Nothing` or `Any` is used, as usual.

#### Simplification Rules

Wildcard type arguments used in covariant or contravariant positions can always be simplified to regular types.

Let ´T[T_1, ..., T_n]´ be a parameterized type for a concrete type constructor.
Then, applying a wildcard type argument ´? >: L <: H´ at the ´i´'th position obeys the following equivalences:

- If the type parameter ´T_i´ is declared covariant, then ´T[..., ? >: L <: H, ...] =:= T[..., H, ...]´.
- If the type parameter ´T_i´ is declared contravariant, then ´T[..., ? >: L <: H, ...] =:= T[..., L, ...]´.

#### Example Parameterized Types

Given the partial type definitions:

```scala
class TreeMap[A <: Comparable[A], B] { ... }
class List[+A] { ... }
class I extends Comparable[I] { ... }

class F[M[A], X] { ... } // M[A] desugars to M <: [A] =>> Any
class S[K <: String] { ... }
class G[M[Z <: I], I] { ... } // M[Z <: I] desugars to M <: [Z <: I] =>> Any
```

the following parameterized types are well-formed:

```scala
TreeMap[I, String]
List[I]
List[List[Boolean]]

F[List, Int]
F[[X] =>> List[X], Int]
G[S, String]

List[?] // ? inferred as List[_ >: Nothing <: Any], equivalent to List[Any]
List[? <: String] // equivalent to List[String]
S[? <: String]
F[?, Boolean] // ? inferred as ? >: Nothing <: [A] =>> Any
```

and the following types are ill-formed:

```scala
TreeMap[I]            // illegal: wrong number of parameters
TreeMap[List[I], Int] // illegal: type parameter not within bound
List[[X] => List[X]]

F[Int, Boolean]       // illegal: Int is not a type constructor
F[TreeMap, Int]       // illegal: TreeMap takes two parameters,
                      //   F expects a constructor taking one
F[[X, Y] => (X, Y)]
G[S, Int]             // illegal: S constrains its parameter to
                      //   conform to String,
                      // G expects type constructor with a parameter
                      //   that conforms to Int
```

The following code also contains an ill-formed type:

```scala
trait H[F[A]]:  // F[A] desugars to F <: [A] =>> Any, which is abstract
  def f: F[_]   // illegal : an abstract type constructor
                // cannot be applied to wildcard arguments.
```

### This Types

```ebnf
ThisType  ::=  classid ‘.‘ ‘this‘
```

A _this type_ `´C´.this` denotes the `this` value of class ´C´ within ´C´.

This types often appear implicitly as the prefix of [designator types](#designator-types) referring to members of ´C´.
They play a particular role in the type system, since they are affected by the [as seen from](#as-seen-from) operation on types.

This types are stable types.
The underlying type of `´C´.this` is the [self type](05-classes-and-objects.html#templates) of ´C´.

### Super Types

```ebnf
SuperType  ::=  classid ‘.‘ ‘super‘ ‘[‘ classid ‘]‘
```

A _super type_ `´C´.super[´D´]` denotes the `this` value of class `C` within `C`, but "widened" to only see members coming from a parent class or trait ´D´.

Super types exist for compatibility with Scala 2, which allows shadowing of inner classes.
In a Scala 3-only context, a super type can always be replaced by the corresponding [this type](#this-types).
Therefore, we omit further discussion of super types in this specification.

### Literal Types

```ebnf
LiteralType  ::=  SimpleLiteral
```

A literal type `lit` denotes the single literal value `lit`.
Thus, the type ascription `1: 1` gives the most precise type to the literal value `1`:  the literal type `1`.

At run time, an expression `e` is considered to have literal type `lit` if `e == lit`.
Concretely, the result of `e.isInstanceOf[lit]` and `e match { case _ : lit => }` is determined by evaluating `e == lit`.

Literal types are available for all primitive types, as well as for `String`.
However, only literal types for `Int`, `Long`, `Float`, `Double`, `Boolean`, `Char` and `String` can be expressed in the concrete syntax.

Literal types are stable types.
Their underlying type is the primitive type containing their value.

##### Example

```scala
val x: 1 = 1
val y: false = false
val z: false = y
val int: Int = x

val badX: 1 = int       // error: Int is not a subtype of 1
val badY: false = true  // error: true is not a subtype of false
```

### By-Name Types

```ebnf
ByNameType  ::=  ‘=>‘ Type
```

A by-name type ´=> T´ denotes the declared type of a by-name term parameter.
By-name types can only appear as the types of parameters in method types, and as type arguments in [parameterized types](#parameterized-types).

<!-- TODO Should by-name types be Types at all? Should we make them similar to MethodicType's instead? -->

### Annotated Types

```ebnf
AnnotatedType  ::=  Type Annotation
```

An _annotated type_ ´T a´ attaches the [annotation](11-annotations.html#user-defined-annotations) ´a´ to the type ´T´.

###### Example

The following type adds the `@suspendable` annotation to the type `String`:

```scala
String @suspendable
```

### Refined Types

```ebnf
RefinedType  ::=  Type ‘{‘ Refinement ‘}‘
Refinement   ::=  ‘type‘ id TypeAliasOrBounds
               |  ‘def‘ id ‘:‘ TypeOrMethodic
               |  ‘val‘ id ‘:‘ Type
```

A _refined type_ ´T { R }´ denotes the set of values that belong to ´T´ and also have a _member_ conforming to the refinement ´R´.

The refined type ´T { R }´ is well-formed if:

- ´T´ is a proper type, and
- if ´R´ is a term (`def` or `val`) refinement, the refined type is a proper type, and
- if ´R´ overrides a member of ´T´, the usual rules for [overriding](05-classes-and-objects.html#overriding) apply, and
- if ´R´ is a `def` refinement with a [polymorphic method type](#polymorphic-method-types), then ´R´ overrides a member definition of ´T´.

As an exception to the last rule, a polymorphic method type refinement is allowed if `´T <:´ scala.PolyFunction` and ´id´ is the name `apply`.

If the refinement ´R´ overrides no member of ´T´ and is not an occurrence of the `scala.PolyFunction` exception, the refinement is said to be “structural” [^2].

[^2]: A reference to a structurally defined member (method call or access to a value or variable) may generate binary code that is significantly slower than an equivalent code to a non-structural member.

Note: since a refinement does not define a _class_, it is not possible to use a [this type](#this-types) to reference term and type members of the parent type ´T´ within the refinement.
When the surface syntax of refined types makes such references, a [recursive type](#recursive-types) wraps the refined type, given access to members of self through a recursive-this type.

###### Example

Given the following class definitions:

```scala
trait T:
  type X <: Option[Any]
  def foo: Any
  def fooPoly[A](x: A): Any

trait U extends T:
  override def foo: Int
  override def fooPoly[A](x: A): A

trait V extends T
  type X = Some[Int]
  def bar: Int
  def barPoly[A](x: A): A
```

We get the following conformance relationships:

- `U <: T { def foo: Int }`
- `U <: T { def fooPoly[A](x: A): A }`
- `U <: (T { def foo: Int }) { def fooPoly[A](x: A): A }` (we can chain refined types to refine multiple members)
- `V <: T { type X <: Some[Any] }`
- `V <: T { type X >: Some[Nothing] }`
- `V <: T { type X = Some[Int] }`
- `V <: T { def bar: Any }` (a structural refinement)

The following refined types are not well-formed:

- `T { def barPoly[A](x: A): A }` (structural refinement for a polymorphic method type)
- `T { type X <: List[Any] }` (does not satisfy overriding rules)
- `List { def head: Int }` (the parent type `List` is not a proper type)
- `T { def foo: List }` (the refined type `List` is not a proper type)
- `T { def foo: T.this.X }` (`T.this` is not allowed outside the body of `T`)

### Recursive Types

```ebnf
RecursiveType  ::=  ‘{‘ recid ‘=>‘ Type ‘}‘
RecursiveThis  ::=  recid ‘.‘ ‘this‘
```

A _recursive type_ of the form `{ ´\alpha´ => ´T´ }` represents the same values as ´T´, while offering ´T´ access to its _recursive this_ type `´\alpha´`.

Recursive types cannot directly be expressed in the concrete syntax.
They are created as needed when a refined type in the concrete syntax contains a refinement that needs access to the `this` value.
Each recursive type defines a unique self-reference `´\alpha´`, distinct from any other recursive type in the system.

Recursive types can be unfolded during subtyping as needed, replacing references to its `´\alpha´` by a stable reference to the other side of the conformance relationship.

##### Example

Given the class definitions in the [refined types](#refined-types) section, we can write the following refined type in the source syntax:

```scala
T { def foo: X }
// equivalent to
T { def foo: this.X }
```

This type is not directly expressible as a refined type alone, as the refinement cannot access the `this` value.
Instead, in the abstract syntax of types, it is translated to `{ ´\alpha´ => ´T´ { def foo: ´\alpha´.X } }`.

Given the following definitions:

```scala
trait Z extends T:
  type X = Option[Int]
  def foo: Option[Int] = Some(5)

val z: Z
```

we can check that `z ´<:´ { ´\alpha´ => ´T´ { def foo: ´\alpha´.X } }`.
We first unfold the recursive type, substituting ´z´ for ´\alpha´, resulting in `z ´<:´ T { def foo: z.X }`.
Since the underlying type of ´z´ is ´Z´, we can resolve `z.X` to mean `Option[Int]`, and then validate that `z ´<:´ T` and that `z` has a member `def foo: Option[Int]`.

### Union and Intersection Types

Syntactically, the types `S | T` and `S & T` are infix types, where the infix operators are `|` and `&`, respectively (see above).

However, in this specification, ´S ｜ T´ and ´S ＆ T´ refer to the underlying core concepts of *union and intersection types*, respectively.

- The type ´S ｜ T´ represents the set of values that are represented by *either* ´S´ or ´T´.
- The type ´S ＆ T´ represents the set of values that are represented by *both* ´S´ and ´T´.

From the [conformance rules](#conformance) rules on union and intersection types, we can show that ´＆´ and ´｜´ are *commutative* and *associative*.
Moreover, `＆` is distributive over `｜`.
For any type ´A´, ´B´ and ´C´, all of the following relationships hold:

- ´A ＆ B \equiv B ＆ A´,
- ´A ｜ B \equiv B ｜ A´,
- ´(A ＆ B) ＆ C \equiv A ＆ (B ＆ C)´,
- ´(A ｜ B) ｜ C \equiv A ｜ (B ｜ C)´, and
- ´A ＆ (B ｜ C) \equiv (A ＆ B) ｜ (A ＆ C)´.

If ´C´ is a type constructor, then ´C[A] ＆ C[B]´ can be simplified using the following three rules:

- If ´C´ is covariant, ´C[A] ＆ C[B] \equiv C[A ＆ B]´
- If ´C´ is contravariant, ´C[A] ＆ C[B] \equiv C[A | B]´
- If ´C´ is invariant, emit a compile error

From the above rules, we can derive the following conformance relationships:

- When ´C´ is covariant, ´C[A ＆ B] <: C[A] ＆ C[B]´.
- When ´C´ is contravariant, ´C[A ｜ B] <: C[A] ＆ C[B]´.

#### Join of a union type

In some situations, a union type might need to be widened to a non-union type.
For this purpose, we define the _join_ of a union type ´T_1 ｜ ... ｜ T_n´ as the smallest intersection type of base class instances of ´T_1, ..., T_n´.
Note that union types might still appear as type arguments in the resulting type, this guarantees that the join is always finite.

For example, given

```scala
trait C[+T]
trait D
trait E
class A extends C[A] with D
class B extends C[B] with D with E
```

The join of ´A ｜ B´ is ´C[A ｜ B] ＆ D´

### Function Types

```ebnf
Type              ::=  FunctionArgs ‘=>’ Type
FunctionArgs      ::=  InfixType
                    |  ‘(’ [ ParamType {‘,’ ParamType } ] ‘)’
```

The type ´(T_1, ..., T_n) \Rightarrow R´ represents the set of function values that take arguments of types ´T_1, ..., Tn´ and yield results of type ´R´.
The case of exactly one argument type ´T \Rightarrow R´ is a shorthand for ´(T) \Rightarrow R´.
An argument type of the form ´\Rightarrow T´ represents a [call-by-name parameter](04-basic-declarations-and-definitions.md#by-name-parameters) of type ´T´.

Function types associate to the right, e.g. ´S \Rightarrow T \Rightarrow R´ is the same as ´S \Rightarrow (T \Rightarrow R)´.

Function types are [covariant](04-basic-declarations-and-definitions.md#variance-annotations) in their result type and [contravariant](04-basic-declarations-and-definitions.md#variance-annotations) in their argument types.

Function types are shorthands for class types that define an `apply` method.
Specifically, the ´n´-ary function type ´(T_1, ..., T_n) \Rightarrow R´ is a shorthand for the class type `Function´_n´[´T_1´, ..., ´T_n´, ´R´]`.
In particular ´() \Rightarrow R´ is a shorthand for class type `Function´_0´[´R´]`.

Such class types behave as if they were instances of the following trait:

```scala
trait Function´_n´[-´T_1´, ..., -´T_n´, +´R´]:
  def apply(´x_1´: ´T_1´, ..., ´x_n´: ´T_n´): ´R´
```

Their exact supertype and implementation can be consulted in the [function classes section](./12-the-scala-standard-library.md#the-function-classes) of the standard library page in this document.

## Non-Value Types

The types explained in the following do not denote sets of values.

### Method Types

A _method type_ is denoted internally as ´(\mathit{Ps})U´, where ´(\mathit{Ps})´ is a sequence of parameter names and types ´(p_1:T_1, ..., p_n:T_n)´ for some ´n \geq 0´ and ´U´ is a (value or method) type.
This type represents named methods that take arguments named ´p_1, ..., p_n´ of types ´T_1, ..., T_n´ and that return a result of type ´U´.

Method types associate to the right: ´(\mathit{Ps}\_1)(\mathit{Ps}\_2)U´ is treated as ´(\mathit{Ps}\_1)((\mathit{Ps}\_2)U)´.

A special case are types of methods without any parameters.
They are written here `=> T`. Parameterless methods name expressions that are re-evaluated each time the parameterless method name is referenced.

<!-- TODO: replace by reference to eta-expansion instead -->
Method types do not exist as types of values.
If a method name is used as a value, its type is [implicitly converted](06-expressions.html#implicit-conversions) to a corresponding function type.

###### Example

The declarations

```scala
def a: Int
def b (x: Int): Boolean
def c (x: Int) (y: String, z: String): String
```

produce the typings

```scala
a: => Int
b: (Int) Boolean
c: (Int) (String, String) String
```

### Polymorphic Method Types

A polymorphic method type is denoted internally as `[´\mathit{tps}\,´]´T´` where `[´\mathit{tps}\,´]` is a type parameter section `[´a_1´ >: ´L_1´ <: ´U_1, ..., a_n´ >: ´L_n´ <: ´U_n´]` for some ´n \geq 0´ and ´T´ is a (value or method) type.
This type represents named methods that take type arguments `´S_1, ..., S_n´` which [conform](#parameterized-types) to the lower bounds `´L_1, ..., L_n´` and the upper bounds `´U_1, ..., U_n´` and that yield results of type ´T´.

###### Example

The declarations

```scala
def empty[A]: List[A]
def union[A <: Comparable[A]] (x: Set[A], xs: Set[A]): Set[A]
```

produce the typings

```scala
empty : [A >: Nothing <: Any] List[A]
union : [A >: Nothing <: Comparable[A]] (x: Set[A], xs: Set[A]) Set[A]
```

### Type Constructors

```
Type             ::=  ... |  TypeLambdaParams ‘=>>’ Type
TypeParamClause  ::=  ‘[’ TypeParam {‘,’ TypeParam} ‘]’
TypeLambdaParams ::=  ‘[’ TypeLambdaParam {‘,’ TypeLambdaParam} ‘]’
TypeLambdaParam  ::=  {Annotation} (id | ‘_’) [TypeParamClause] [‘>:’ Type] [‘<:’ Type]
```

<!-- the definition of a parameterized type above uses the concept of a type constructor, so we can't define a type constructor as an unapplied parameterized type. -->

A _type constructor_ is either:
- a _type lambda_, of the form `[´\mathit{tps}\,´] =>> ´T´` where `[´\mathit{tps}\,´]` is a type parameter clause `[´a_1´ >: ´L_1´ <: ´U_1, ..., a_n´ >: ´L_n´ <: ´U_n´]` for some ´n \gt 0´ and ´T´ is either a value type
or another type lambda.
- a reference to a [desugared type declaration](04-basic-declarations-and-definitions.html#type-declarations-and-type-aliases) upper-bounded by a type lambda.
- a reference to a [polymorphic class](05-classes-and-objects.html##class-definitions).

Each type parameter ´a_i´ of a type lambda has a variance ´v_i´ which cannot be written down by the user but is inferred from the body of the type lambda to maximize the number of types that conform to the type lambda.
<!-- TODO: write down the exact algorithm? -->

#### Inferred type parameter clause

To each type constructor corresponds an _inferred type parameter clause_ which is computed as follow:
- For a type lambda, its type parameter clause (including variance annotations).
- For a type declaration upper-bounded by a type lambda ´T´, the inferred clause of ´T´.
- For a polymorphic class, its type parameter clause.

<!-- ### Overloaded Types

More than one values or methods are defined in the same scope with the
same name, we model

An overloaded type consisting of type alternatives ´T_1 \commadots T_n (n \geq 2)´ is denoted internally ´T_1 \overload ... \overload T_n´.

###### Example
```scala
def println(): Unit
def println(s: String): Unit = ...
def println(x: Float): Unit = ...
def println(x: Float, width: Int): Unit = ...
def println[A](x: A)(tostring: A => String): Unit = ...
```
define a single function `println` which has an overloaded
type.
```
println:  () Unit ´\overload´
          (String) Unit ´\overload´
          (Float) Unit ´\overload´
          (Float, Int) Unit ´\overload´
          [A] (A) (A => String) Unit
```

###### Example
```scala
def f(x: T): T = ...
val f = 0
```
define a function `f} which has type `(x: T)T ´\overload´ Int`.
-->

## Kind Polymorphism

Type parameters are normally partitioned into _kinds_, indicated by the top type of which it is a subtype.
Proper types are the types of values and are subtypes of `Any`.
Higher-kinded types are type constructors such as `List` or `Map`.
Covariant single argument type constructors such as `List` are subtypes of `[+X] =>> Any`.
The `Map` type constructor is a subtype of `[X, +Y] =>> Any`.

A type can be used only as prescribed by its kind.
Subtypes of `Any` cannot be applied to type arguments whereas subtypes of `[X] =>> Any` _must_ be applied to a type argument, unless they are passed to type parameters of the same kind.

A type parameter whose upper bound is [`scala.AnyKind`](https://scala-lang.org/api/3.x/scala/AnyKind.html) can have any kind and is called an _any-kinded type_.

```scala
def f[T <: AnyKind] = ...
```

The actual type arguments of `f` can then be types of arbitrary kinds.
So the following are all legal:

```scala
f[Int]
f[List]
f[Map]
f[[X] =>> String]
```

Since the actual kind of an any-kinded type is unknown, its usage is heavily restricted.
An any-kinded type can neither be the type of a value, nor be instantiated with type parameters.
The only thing one can do with an any-kinded type is to pass it to another any-kinded type argument.

`AnyKind` plays a special role in Scala's subtype system.
It is a supertype of all other types, no matter what their kind is.
It is also assumed to be kind-compatible with all other types.
Furthermore, `AnyKind` is itself an any-kinded type, so it cannot be the type of values and it cannot be instantiated.

## Base Types and Member Definitions

Types of class members depend on the way the members are referenced.
Central here are three notions, namely:
1. the notion of the set of base types of a type ´T´,
1. the notion of a type ´T´ in some class ´C´ seen from some
   prefix type ´S´,
1. the notion of the set of member bindings of some type ´T´.

These notions are defined mutually recursively as follows.

1. The set of _base types_ of a type is a set of class types,
   given as follows.
  - The base types of a class type ´C´ with parents ´T_1, ..., T_n´ are ´C´ itself, as well as the base types of the compound type `´T_1´ with ... with ´T_n´ { ´R´ }`.
  - The base types of an aliased type are the base types of its alias.
  - The base types of an abstract type are the base types of its upper bound.
  - The base types of a parameterized type `´C´[´T_1, ..., T_n´]` are the base types of type ´C´, where every occurrence of a type parameter ´a_i´ of ´C´ has been replaced by the corresponding parameter type ´T_i´.
  - The base types of a singleton type `´p´.type` are the base types of the type of ´p´.
  - The base types of a compound type `´T_1´ with ... with ´T_n´ { ´R´ }` are the _reduced union_ of the base classes of all ´T_i´'s.
  This means: Let the multi-set ´\mathscr{S}´ be the multi-set-union of the base types of all ´T_i´'s.
  If ´\mathscr{S}´ contains several type instances of the same class, say `´S^i´#´C´[´T^i_1, ..., T^i_n´]` ´(i \in I)´, then all those instances are replaced by one of them which conforms to all others.
  It is an error if no such instance exists.
  It follows that the reduced union, if it exists, produces a set of class types, where different types are instances of different classes.
  - The base types of a type selection `´S´#´T´` are determined as follows.
  If ´T´ is an alias or abstract type, the previous clauses apply.
  Otherwise, ´T´ must be a (possibly parameterized) class type, which is defined in some class ´B´.
  Then the base types of `´S´#´T´` are the base types of ´T´ in ´B´ seen from the prefix type ´S´.

1. The notion of a type ´T´ _in class ´C´ seen from some prefix type ´S´_ makes sense only if the prefix type ´S´ has a type instance of class ´C´ as a base type, say `´S'´#´C´[´T_1, ..., T_n´]`.
Then we define as follows.
  - If `´S´ = ´\epsilon´.type`, then ´T´ in ´C´ seen from ´S´ is ´T´ itself.
  - Otherwise, if ´T´ is the ´i´'th type parameter of some class ´D´, then
    - If ´S´ has a base type `´D´[´U_1, ..., U_n´]`, for some type parameters `[´U_1, ..., U_n´]`, then ´T´ in ´C´ seen from ´S´ is ´U_i´.
    - Otherwise, if ´C´ is defined in a class ´C'´, then ´T´ in ´C´ seen from ´S´ is the same as ´T´ in ´C'´ seen from ´S'´.
    - Otherwise, if ´C´ is not defined in another class, then ´T´ in ´C´ seen from ´S´ is ´T´ itself.
  - Otherwise, if ´T´ is the singleton type `´D´.this.type` for some class ´D´ then
    - If ´D´ is a subclass of ´C´ and ´S´ has a type instance of class ´D´ among its base types, then ´T´ in ´C´ seen from ´S´ is ´S´.
    - Otherwise, if ´C´ is defined in a class ´C'´, then ´T´ in ´C´ seen from ´S´ is the same as ´T´ in ´C'´ seen from ´S'´.
    - Otherwise, if ´C´ is not defined in another class, then ´T´ in ´C´ seen from ´S´ is ´T´ itself.
  - If ´T´ is some other type, then the described mapping is performed to all its type components.

If ´T´ is a possibly parameterized class type, where ´T´'s class is defined in some other class ´D´, and ´S´ is some prefix type, then we use "´T´ seen from ´S´" as a shorthand for "´T´ in ´D´ seen from ´S´".

1. The _member bindings_ of a type ´T´ are
  1. all bindings ´d´ such that there exists a type instance of some class ´C´ among the base types of ´T´ and there exists a definition or declaration ´d'´ in ´C´ such that ´d´ results from ´d'´ by replacing every type ´T'´ in ´d'´ by ´T'´ in ´C´ seen from ´T´, and
  2. all bindings of the type's [refinement](#compound-types), if it has one.
2. The member bindinds of ´S ＆ T´ are all the binds of ´S´ *and* all the bindins of ´T´.
3. The member bindings of ´S ｜ T´ are the member bindings of its [join](#join-of-a-union-type).

The _definition_ of a type projection `S#T` is the member binding ´d_T´ of the type `T` in `S`.
In that case, we also say that `S#T` _is defined by_ ´d_T´.

## Relations between types

We define the following relations between types.

| Name             | Symbolically   | Interpretation                                     |
|------------------|----------------|----------------------------------------------------|
| Equivalence      | ´T \equiv U´   | ´T´ and ´U´ are interchangeable in all contexts.   |
| Conformance      | ´T <: U´       | Type ´T´ conforms to ("is a subtype of") type ´U´. |
| Weak Conformance | ´T <:_w U´     | Augments conformance for primitive numeric types.  |
| Compatibility    |                | Type ´T´ conforms to type ´U´ after conversions.   |

### Equivalence

´\color{red}{\text{TODO SCALA3: Redefine equivalence as mutual conformance?}}´

Equivalence ´(\equiv)´ between types is the smallest congruence [^congruence] such that the following holds:

- If ´t´ is defined by a type alias `type ´t´ = ´T´`, then ´t´ is equivalent to ´T´.
- If a path ´p´ has a singleton type `´q´.type`, then `´p´.type ´\equiv q´.type`.
- If ´O´ is defined by an object definition, and ´p´ is a path consisting only of package or object selectors and ending in ´O´, then `´O´.this.type ´\equiv p´.type`.
- Two [compound types](#compound-types) are equivalent if the sequences of their component are pairwise equivalent, and occur in the same order, and their refinements are equivalent. Two refinements are equivalent if they bind the same names and the modifiers, types and bounds of every declared entity are equivalent in both refinements.
- Two [method types](#method-types) are equivalent if:
  - neither are implicit, or they both are [^implicit];
  - they have equivalent result types;
  - they have the same number of parameters; and
  - corresponding parameters have equivalent types.
    Note that the names of parameters do not matter for method type equivalence.
- Two [polymorphic method types](#polymorphic-method-types) are equivalent if they have the same number of type parameters, and, after renaming one set of type parameters by another, the result types as well as lower and upper bounds of corresponding type parameters are equivalent.
- Two [type constructors](#type-constructors) are equivalent if they have the same number of type parameters, and, after renaming one list of type parameters by another, the result types as well as variances, lower and upper bounds of corresponding type parameters are equivalent.

[^congruence]: A congruence is an equivalence relation which is closed under formation of contexts.
[^implicit]: A method type is implicit if the parameter section that defines it starts with the `implicit` keyword.

### Conformance

The conformance relation ´(<:)´ is the smallest transitive relation that satisfies the following conditions.

- Conformance includes equivalence. If ´T \equiv U´ then ´T <: U´.
- For every type `´T´` (of any kind), `scala.Nothing <: ´T´ <: scala.AnyKind`.
- For every value type `´T´`, `´T´ <: scala.Any`.
- For every type constructor `´T´` with type parameters `[´U_1´, ..., ´U_n´]`, `[´U_1´, ..., ´U_n´] =>> scala.Nothing <: ´T´ <: [´U_1´, ..., ´U_n´] =>> scala.Any`.
- For every value type ´T´, `scala.Null <: ´T´` unless `´T´ <: scala.AnyVal`.
- A type variable or abstract type ´t´ conforms to its upper bound and its lower bound conforms to ´t´.
- A class type or parameterized type conforms to any of its base-types.
- A singleton type `´p´.type` conforms to the type of the path ´p´.
- A singleton type `´p´.type` conforms to the type `scala.Singleton`.
- A type projection `´T´#´t´` conforms to `´U´#´t´` if ´T´ conforms to ´U´.
- A parameterized type `´T´[´T_1´, ..., ´T_n´]` conforms to `´T´[´U_1´, ..., ´U_n´]` if the following conditions hold for ´i \in \{ 1, ..., n \}´:
  1. If the ´i´'th type parameter of ´T´ is declared covariant, then ´T_i <: U_i´. [^argisnotwildcard]
  1. If the ´i´'th type parameter of ´T´ is declared contravariant, then ´U_i <: T_i´. [^argisnotwildcard]
  1. If the ´i´'th type parameter of ´T´ is declared neither covariant nor contravariant:
     1. If neither ´T_i´ nor ´U_i´ are wildcard type arguments, then ´U_i \equiv T_i´.
     1. If ´T_i´ is a wildcard type argument of the form ´\\_ >: L_1 <: U_1´ and ´U_i´ is a wildcard argument of the form ´\\_ >: L_2 <: U_2´, then ´L_2 <: L_1´ and ´H_1 <: H_2´ (i.e., the ´T_i´ "interval" is contained in the ´U_i´ "interval").
     1. If ´U_i´ is a wildcard type argument of the form ´\\_ >: L_2 <: U_2´, then ´L_2 <: T_i´ and ´T_i <: U_2´.
- A compound type `´T_1´ with ... with ´T_n´ {´R\,´}` conforms to each of its component types ´T_i´.
- If ´T <: U_i´ for ´i \in \{ 1, ..., n \}´ and for every binding ´d´ of a type or value ´x´ in ´R´ there exists a member binding of ´x´ in ´T´ which subsumes ´d´, then ´T´ conforms to the compound type `´U_1´ with ... with ´U_n´ {´R\,´}`.
- If ´T <: U´, then ´T <: U ｜ W´ and ´T <: W ｜ U´.
- If ´T <: W´ and ´U <: W´, then ´T ｜ U <: W´.
- If ´T <: U´ and ´T <: W´, then ´T <: U ＆ W´.
- If ´T <: W´, then ´T ＆ U <: W´ and ´U ＆ T <: W´.
- If ´T_i \equiv T_i'´ for ´i \in \{ 1, ..., n\}´ and ´U´ conforms to ´U'´ then the method type ´(p_1:T_1, ..., p_n:T_n) U´ conforms to ´(p_1':T_1', ..., p_n':T_n') U'´.
- The polymorphic type ´[a_1 >: L_1 <: U_1, ..., a_n >: L_n <: U_n] T´ conforms to the polymorphic type ´[a_1 >: L_1' <: U_1', ..., a_n >: L_n' <: U_n'] T'´ if, assuming ´L_1' <: a_1 <: U_1', ..., L_n' <: a_n <: U_n'´ one has ´T <: T'´ and ´L_i <: L_i'´ and ´U_i' <: U_i´ for ´i \in \{ 1, ..., n \}´.
- Type constructors ´T´ and ´T'´ follow a similar discipline.
We characterize ´T´ and ´T'´ by their [inferred type parameter clauses](#inferred-type-parameter-clause) ´[a_1, ..., a_n]´ and ´[a_1', ..., a_n']´.
Then, ´T´ conforms to ´T'´ if any list ´[t_1, ..., t_n]´ -- with declared variances, bounds and higher-order type parameter clauses -- of valid type arguments for ´T'´ is also a valid list of type arguments for ´T´ and ´T[t_1, ..., t_n] <: T'[t_1, ..., t_n]´.
Note that this entails that:
  - The bounds on ´a_i´ must be weaker than the corresponding bounds declared for ´a'_i´.
  - The variance of ´a_i´ must match the variance of ´a'_i´, where covariance matches covariance, contravariance matches contravariance and any variance matches invariance.
  - Recursively, these restrictions apply to the corresponding higher-order type parameter clauses of ´a_i´ and ´a'_i´.

 [^argisnotwildcard]: In these cases, if `T_i` and/or `U_i` are wildcard type arguments, the [simplification rules](#simplification-rules) for parameterized types allow to reduce them to real types.

A declaration or definition in some compound type of class type ´C´ _subsumes_ another declaration of the same name in some compound type or class type ´C'´, if one of the following holds.

- A value declaration or definition that defines a name ´x´ with type ´T´ subsumes a value or method declaration that defines ´x´ with type ´T'´, provided ´T <: T'´.
- A method declaration or definition that defines a name ´x´ with type ´T´ subsumes a method declaration that defines ´x´ with type ´T'´, provided ´T <: T'´.
- A type alias `type ´t´[´T_1´, ..., ´T_n´] = ´T´` subsumes a type alias `type ´t´[´T_1´, ..., ´T_n´] = ´T'´` if ´T \equiv T'´.
- A type declaration `type ´t´[´T_1´, ..., ´T_n´] >: ´L´ <: ´U´` subsumes a type declaration `type ´t´[´T_1´, ..., ´T_n´] >: ´L'´ <: ´U'´` if ´L' <: L´ and ´U <: U'´.
- A type or class definition that binds a type name ´t´ subsumes an abstract type declaration `type t[´T_1´, ..., ´T_n´] >: L <: U` if ´L <: t <: U´.

#### Least upper bounds and greatest lower bounds

The ´(<:)´ relation forms pre-order between types, i.e. it is transitive and reflexive.
This allows us to define _least upper bounds_ and _greatest lower bounds_ of a set of types in terms of that order.

- the _least upper bound_ of `A` and `B` is the smallest type `L` such that `A` <: `L` and `B` <: `L`.
- the _greatest lower bound_ of `A` and `B` is the largest type `G` such that `G` <: `A` and `G` <: `B`.

By construction, for all types `A` and `B`, the least upper bound of `A` and `B` is `A ｜ B`, and their greatest lower bound is `A ＆ B`.

### Weak Conformance

In some situations Scala uses a more general conformance relation.
A type ´S´ _weakly conforms_ to a type ´T´, written ´S <:_w T´, if ´S <: T´ or both ´S´ and ´T´ are primitive number types and ´S´ precedes ´T´ in the following ordering.

```scala
Byte  ´<:_w´ Short
Short ´<:_w´ Int
Char  ´<:_w´ Int
Int   ´<:_w´ Long
Long  ´<:_w´ Float
Float ´<:_w´ Double
```

A _weak least upper bound_ is a least upper bound with respect to weak conformance.

### Compatibility
A type ´T´ is _compatible_ to a type ´U´ if ´T´ (or its corresponding function type) [weakly conforms](#weak-conformance) to ´U´ after applying [eta-expansion](06-expressions.html#eta-expansion).
If ´T´ is a method type, it's converted to the corresponding function type.
If the types do not weakly conform, the following alternatives are checked in order:
- dropping by-name modifiers: if ´U´ is of the shape `´=> U'´` (and ´T´ is not), `´T <:_w U'´`;
- SAM conversion: if ´T´ corresponds to a function type, and ´U´ declares a single abstract method whose type [corresponds](06-expressions.html#sam-conversion) to the function type ´U'´, `´T <:_w U'´`.
- [implicit conversion](07-implicits.html#views): there's an implicit conversion from ´T´ to ´U´ in scope;

#### Examples

##### Function compatibility via SAM conversion

Given the definitions

```scala
def foo(x: Int => String): Unit
def foo(x: ToString): Unit

trait ToString { def convert(x: Int): String }
```

The application `foo((x: Int) => x.toString)` [resolves](06-expressions.html#overloading-resolution) to the first overload, as it's more specific:
- `Int => String` is compatible to `ToString` -- when expecting a value of type `ToString`, you may pass a function literal from `Int` to `String`, as it will be SAM-converted to said function;
- `ToString` is not compatible to `Int => String` -- when expecting a function from `Int` to `String`, you may not pass a `ToString`.

## Volatile Types

Type volatility approximates the possibility that a type parameter or abstract type instance of a type does not have any non-null values.
A value member of a volatile type cannot appear in a [path](#paths).

A type is _volatile_ if it falls into one of four categories:

A compound type `´T_1´ with ... with ´T_n´ {´R\,´}` is volatile if one of the following three conditions hold.

1. One of ´T_2, ..., T_n´ is a type parameter or abstract type, or
1. ´T_1´ is an abstract type and either the refinement ´R´ or a type ´T_j´ for ´j > 1´ contributes an abstract member to the compound type, or
1. one of ´T_1, ..., T_n´ is a singleton type.

Here, a type ´S´ _contributes an abstract member_ to a type ´T´ if ´S´ contains an abstract member that is also a member of ´T´.
A refinement ´R´ contributes an abstract member to a type ´T´ if ´R´ contains an abstract declaration which is also a member of ´T´.

A type designator is volatile if it is an alias of a volatile type, or if it designates a type parameter or abstract type that has a volatile type as its upper bound.

A singleton type `´p´.type` is volatile, if the underlying type of path ´p´ is volatile.

## Type Erasure

A type is called _generic_ if it contains type arguments or type variables.
_Type erasure_ is a mapping from (possibly generic) types to non-generic types.
We write ´|T|´ for the erasure of type ´T´.
The erasure mapping is defined as follows.

- The erasure of `scala.AnyKind` is `Object`.
- The erasure of an alias type is the erasure of its right-hand side.
- The erasure of an abstract type is the erasure of its upper bound.
- The erasure of the parameterized type `scala.Array´[T_1]´` is `scala.Array´[|T_1|]´`.
- The erasure of every other parameterized type ´T[T_1, ..., T_n]´ is ´|T|´.
- The erasure of a singleton type `´p´.type` is the erasure of the type of ´p´.
- The erasure of a type projection `´T´#´x´` is `|´T´|#´x´`.
- The erasure of a compound type `´T_1´ with ... with ´T_n´ {´R\,´}` is the erasure of the intersection dominator of ´T_1, ..., T_n´.
- The erasure of a union type ´S ｜ T´ is the _erased least upper bound_ (_elub_) of the erasures of ´S´ and ´T´.
- The erasure of an intersection type ´S ＆ T´ is the _eglb_ (erased greatest lower bound) of the erasures of ´S´ and ´T´.

The erased LUB is computed as follows:

- if both argument are arrays of objects, an array of the erased LUB of the element types
- if both arguments are arrays of same primitives, an array of this primitive
- if one argument is array of primitives and the other is array of objects, [`Object`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/Object.html)
- if one argument is an array, [`Object`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/Object.html)
- otherwise a common superclass or trait S of the argument classes, with the following two properties:
  - S is minimal: no other common superclass or trait derives from S, and
  - S is last: in the linearization of the first argument type ´|A|´ there are no minimal common superclasses or traits that come after S.
  The reason to pick last is that we prefer classes over traits that way, which leads to more predictable bytecode and (?) faster dynamic dispatch.

The rules for ´eglb(A, B)´ are given below in pseudocode:

```
eglb(scala.Array[A], JArray[B]) = scala.Array[eglb(A, B)]
eglb(scala.Array[T], _)         = scala.Array[T]
eglb(_, scala.Array[T])         = scala.Array[T]
eglb(A, B)                      = A                     if A extends B
eglb(A, B)                      = B                     if B extends A
eglb(A, _)                      = A                     if A is not a trait
eglb(_, B)                      = B                     if B is not a trait
eglb(A, _)                      = A                     // use first
```
