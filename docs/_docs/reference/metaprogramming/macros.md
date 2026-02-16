---
layout: doc-page
title: "Macros"
nightlyOf: https://docs.scala-lang.org/scala3/reference/metaprogramming/macros.html
---

> When developing macros enable `-Xcheck-macros` scalac option flag to have extra runtime checks.

## Multi-Staging

#### Quoted expressions
Multi-stage programming in Scala 3 uses quotes `'{..}` to delay, i.e., stage, execution of code and splices `${..}` to evaluate and insert code into quotes.
Quoted expressions are typed as `Expr[T]` with a covariant type parameter `T`.
It is easy to write statically safe code generators with these two concepts.
The following example shows a naive implementation of the $x^n$ mathematical operation.

```scala
import scala.quoted.*
def unrolledPowerCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
  if n == 0 then '{ 1.0 }
  else if n == 1 then x
  else '{ $x * ${ unrolledPowerCode(x, n-1) } }
```

```scala
'{
  val x = ...
  ${ unrolledPowerCode('{x}, 3) } // evaluates to: x * x * x
}
```

Quotes and splices are duals of each other.
For an arbitrary expression `x` of type `T` we have `${'{x}} = x` and for an arbitrary expression `e` of type `Expr[T]` we have `'{${e}} = e`.

#### Abstract types
Quotes can handle generic and abstract types using the type class `Type[T]`.
A quote that refers to a generic or abstract type `T` requires a given `Type[T]` to be provided in the implicit scope.
The following examples show how `T` is annotated with a context bound (`: Type`) to provide an implicit `Type[T]`, or the equivalent `using Type[T]` parameter.

```scala
import scala.quoted.*
def singletonListExpr[T: Type](x: Expr[T])(using Quotes): Expr[List[T]] =
  '{ List[T]($x) } // generic T used within a quote

def emptyListExpr[T](using Type[T], Quotes): Expr[List[T]] =
  '{ List.empty[T] } // generic T used within a quote
```

If no other instance is found, the default `Type.of[T]` is used.
The following example implicitly uses `Type.of[String]` and `Type.of[Option[U]]`.
```scala
val list1: Expr[List[String]] =
  singletonListExpr('{"hello"}) // requires a given `Type[Sting]`
val list0: Expr[List[Option[T]]] =
  emptyListExpr[Option[U]] // requires a given `Type[Option[U]]`
```


The `Type.of[T]` method is a primitive operation that the compiler will handle specially.
It will provide the implicit if the type `T` is statically known, or if `T` contains some other types `Ui` for which we have an implicit `Type[Ui]`.
In the example, `Type.of[String]` has a statically known type and `Type.of[Option[U]]` requires an implicit `Type[U]` in scope.

#### Quote context
We also track the current quotation context using a given `Quotes` instance.
To create a quote `'{..}` we require a given `Quotes` context, which should be passed as a contextual parameter `(using Quotes)` to the function.
Each splice will provide a new `Quotes` context within the scope of the splice.
Therefore quotes and splices can be seen as methods with the following signatures, but with special semantics.
```scala
def '[T](x: T): Quotes ?=> Expr[T] // def '[T](x: T)(using Quotes): Expr[T]

def $[T](x: Quotes ?=> Expr[T]): T
```

The lambda with a question mark `?=>` is a contextual function; it is a lambda that takes its argument implicitly and provides it implicitly in the implementation the lambda.
`Quotes` are used for a variety of purposes that will be mentioned when covering those topics.

## Quoted Values

#### Lifting
While it is not possible to use cross-stage persistence of local variables, it is possible to lift them to the next stage.
To this end, we provide the `Expr.apply` method, which can take a value and lift it into a quoted representation of the value.

```scala
val expr1plus1: Expr[Int] = '{ 1 + 1 }

val expr2: Expr[Int] = Expr(1 + 1) // lift 2 into '{ 2 }
```

While it looks type wise similar to `'{ 1 + 1 }`, the semantics of `Expr(1 + 1)` are quite different.
`Expr(1 + 1)` will not stage or delay any computation; the argument is evaluated to a value and then lifted into a quote.
The quote will contain code that will create a copy of this value in the next stage.
`Expr` is polymorphic and user-extensible via the `ToExpr` type class.

```scala
trait ToExpr[T]:
  def apply(x: T)(using Quotes): Expr[T]
```

We can implement a `ToExpr` using a `given` definition that will add the definition to the implicits in scope.
In the following example we show how to implement a `ToExpr[Option[T]]` for any liftable type `T.

```scala
given OptionToExpr: [T: {Type, ToExpr}] => ToExpr[Option[T]]:
  def apply(opt: Option[T])(using Quotes): Expr[Option[T]] =
    opt match
      case Some(x) => '{ Some[T]( ${Expr(x)} ) }
      case None => '{ None }
```

The `ToExpr` for primitive types must be implemented as primitive operations in the system.
In our case, we use the reflection API to implement them.

#### Extracting values from quotes
To be able to generate optimized code using the method `unrolledPowerCode`, the macro implementation `powerCode` needs to first
determine whether the argument passed as parameter `n` is a known constant value.
This can be achieved via _unlifting_ using the `Expr.unapply` extractor from our library implementation, which will only match if `n` is a quoted constant and extracts its value.

```scala
def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
  n match
    case Expr(m) => // it is a constant: unlift code n='{m} into number m
      unrolledPowerCode(x, m)
    case _ => // not known: call power at run-time
      '{ power($x, $n) }
```

Alternatively, the `n.value` method can be used to get an `Option[Int]` with the value or `n.valueOrAbort` to get the value directly.
```scala
def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
  // emits an error message if `n` is not a constant
  unrolledPowerCode(x, n.valueOrAbort)
```

`Expr.unapply` and all variants of `value` are polymorphic and user-extensible via a given `FromExpr` type class.

```scala
trait FromExpr[T]:
  def unapply(x: Expr[T])(using Quotes): Option[T]
```

We can use `given` definitions to implement the `FromExpr` as we did for `ToExpr`.
The `FromExpr` for primitive types must be implemented as primitive operations in the system.
In our case, we use the reflection API to implement them.
To implement `FromExpr` for non-primitive types we use quote pattern matching (for example `OptionFromExpr`).


## Macros and Multi-Stage Programming

The system supports multi-stage macros and run-time multi-stage programming using the same quotation abstractions.

### Multi-Stage Macros

#### Macros
We can generalize the splicing abstraction to express macros.
A macro consists of a top-level splice that is not nested in any quote.
Conceptually, the contents of the splice are evaluated one stage earlier than the program.
In other words, the contents are evaluated while compiling the program. The generated code resulting from the macro replaces the splice in the program.

```scala
def power2(x: Double): Double =
  ${ unrolledPowerCode('x, 2) } // x * x
```

#### Inline macros
Since using the splices in the middle of a program is not as ergonomic as calling a function; we hide the staging mechanism from end-users of macros. We have a uniform way of calling macros and normal functions.
For this, _we restrict the use of top-level splices to only appear in inline methods_[^1][^2].

```scala
// inline macro definition
inline def powerMacro(x: Double, inline n: Int): Double =
  ${ powerCode('x, 'n) }

// user code
def power2(x: Double): Double =
  powerMacro(x, 2) // x * x
```

The evaluation of the macro will only happen when the code is inlined into `power2`.
When inlined, the code is equivalent to the previous definition of `power2`.
A consequence of using inline methods is that none of the arguments nor the return type of the macro will have to mention the `Expr` types; this hides all aspects of metaprogramming from the end-users.

#### Avoiding a complete interpreter
When evaluating a top-level splice, the compiler needs to interpret the code that is within the splice.
Providing an interpreter for the entire language is quite tricky, and it is even more challenging to make that interpreter run efficiently.
To avoid needing a complete interpreter, we can impose the following restrictions on splices to simplify the evaluation of the code in top-level splices.
 * The top-level splice must contain a single call to a compiled static method.
 * Arguments to the function are literal constants, quoted expressions (parameters), calls to `Type.of` for type parameters and a reference to `Quotes`.

In particular, these restrictions disallow the use of splices in top-level splices.
Such a splice would require several stages of interpretation which would be unnecessarily inefficient.

#### Compilation stages
The macro implementation (i.e., the method called in the top-level splice) can come from any pre-compiled library.
This provides a clear difference between the stages of the compilation process.
Consider the following 3 source files defined in distinct libraries.
```scala
// Macro.scala
def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] = ...
inline def powerMacro(x: Double, inline n: Int): Double =
  ${ powerCode('x, 'n) }
```

```scala
// Lib.scala (depends on Macro.scala)
def power2(x: Double) =
  ${ powerCode('x, '{2}) } // inlined from a call to: powerMacro(x, 2)
```

```scala
// App.scala  (depends on Lib.scala)
@main def app() = power2(3.14)
```
One way to syntactically visualize this is to put the application in a quote that delays the compilation of the application.
Then the application dependencies can be placed in an outer quote that contains the quoted application, and we repeat this recursively for dependencies of dependencies.

```scala
'{ // macro library (compilation stage 1)
  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
    ...
  inline def powerMacro(x: Double, inline n: Int): Double =
    ${ powerCode('x, 'n) }
  '{ // library using macros (compilation stage 2)
    def power2(x: Double) =
      ${ powerCode('x, '{2}) } // inlined from a call to: powerMacro(x, 2)
    '{ power2(3.14) /* app (compilation stage 3) */ }
  }
}
```

To make the system more versatile, we allow calling macros in the project where it is defined, with some restrictions.
For example, to compile `Macro.scala` and `Lib.scala` together in the same library.
To this end, we do not follow the simpler syntactic model and rely on semantic information from the source files.
When compiling a source, if we detect a call to a macro that is not compiled yet, we delay the compilation of this source to the following compilation stage.
In the example, we would delay the compilation of `Lib.scala` because it contains a compile-time call to `powerCode`.
Compilation stages are repeated until all sources are compiled, or no progress can be made.
If no progress is made, there was a cyclic dependency between the definition and the use of the macro.
We also need to detect if at runtime the macro depends on sources that have not been compiled yet.
These are detected by executing the macro and checking for JVM linking errors to classes that have not been compiled yet.

### Run-Time Multi-Stage Programming

See [Run-Time Multi-Stage Programming](./staging.md)

## Safety

Multi-stage programming is by design statically safe and cross-stage safe.

### Static Safety

#### Hygiene
All identifier names are interpreted as symbolic references to the corresponding variable in the context of the quote.
Therefore, while evaluating the quote, it is not possible to accidentally rebind a reference to a new variable with the same textual name.

#### Well-typed
If a quote is well typed, then the generated code is well typed.
This is a simple consequence of tracking the type of each expression.
An `Expr[T]` can only be created from a quote that contains an expression of type `T`.
Conversely, an `Expr[T]` can only be spliced in a location that expects a type `T`.
As mentioned before, `Expr` is covariant in its type parameter.
This means that an `Expr[T]` can contain an expression of a subtype of `T`.
When spliced in a location that expects a type `T, these expressions also have a valid type.

### Cross-Stage Safety

#### Level consistency
We define the _staging level_ of some code as the number of quotes minus the number of splices surrounding said code.
Local variables must be defined and used in the same staging level.

It is never possible to access a local variable from a lower staging level as it does not yet exist.

```scala
def badPower(x: Double, n: Int): Double =
  ${ unrolledPowerCode('x, n) } // error: value of `n` not known yet
```


In the context of macros and _cross-platform portability_, that is,
macros compiled on one machine but potentially executed on another,
we cannot support cross-stage persistence of local variables.
Therefore, local variables can only be accessed at precisely the same staging level in our system.

```scala
def badPowerCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
  // error: `n` potentially not available in the next execution environment
  '{ power($x, n) }
```


The rules are slightly different for global definitions, such as `unrolledPowerCode`.
It is possible to generate code that contains a reference to a _global_ definition such as in `'{ power(2, 4) }`.
This is a limited form of cross-stage persistence that does not impede cross-platform portability, where we refer to the already compiled code for `power`.
Each compilation step will lower the staging level by one while keeping global definitions.
In consequence, we can refer to compiled definitions in macros such as `unrolledPowerCode` in `${ unrolledPowerCode('x, 2) }`.

We can sumarize level consistency in two rules:
 * Local variables can be used only at the same staging level as their definition
 * Global variables can be used at any staging level


#### Type consistency
As Scala uses type erasure, generic types will be erased at run-time and hence in any following stage.
To ensure any quoted expression that refers to a generic type `T` does not lose the information it needs, we require a given `Type[T]` in scope.
The `Type[T]` will carry over the non-erased representation of the type into the next phase.
Therefore any generic type used at a higher staging level than its definition will require its `Type`.

#### Scope extrusion
Within the contents of a splice, it is possible to have a quote that refers to a local variable defined in the outer quote.
If this quote is used within the splice, the variable will be in scope.
However, if the quote is somehow _extruded_ outside the splice, then variables might not be in scope anymore.
Quoted expressions can be extruded using side effects such as mutable state and exceptions.
The following example shows how a quote can be extruded using mutable state.
```scala
var x: Expr[T] = null
'{ (y: T) => ${ x = 'y; 1 } }
x // has value '{y} but y is not in scope
```

A second way a variable can be extruded is through the `run` method.
If `run` consumes a quoted variable reference, it will not be in scope anymore.
The result will reference a variable that is defined in the next stage.

```scala
'{ (x: Int) => ${ run('x); ... } }
// evaluates to: '{ (x: Int) => ${ x; ... } 1
```

To catch both scope extrusion scenarios, our system restricts the use of quotes by only allowing a quote to be spliced if it was not extruded from a splice scope.
Unlike level consistency, this is checked at run-time[^4] rather than compile-time to avoid making the static type system too complicated.

Each `Quotes` instance contains a unique scope identifier and refers to its parent scope, forming a stack of identifiers.
The parent of the scope of a `Quotes` is the scope of the `Quotes` used to create the enclosing quote.
Top-level splices and `run` create new scope stacks.
Every `Expr` knows in which scope it was created.
When it is spliced, we check that the quote scope is either the same as the splice scope, or a parent scope thereof.


## Staged Lambdas

When staging programs in a functional language there are two fundamental abstractions: a staged lambda `Expr[T => U]` and a staging lambda `Expr[T] => Expr[U]`.
The first is a function that will exist in the next stage, whereas the second is a function that exists in the current stage.
It is often convenient to have a mechanism to go from `Expr[T => U]` to `Expr[T] => Expr[U]` and vice versa.

```scala
def later[T: Type, U: Type](f: Expr[T] => Expr[U]): Expr[T => U] =
  '{ (x: T) => ${ f('x) } }

def now[T: Type, U: Type](f: Expr[T => U]): Expr[T] => Expr[U] =
  (x: Expr[T]) => '{ $f($x) }
```

Both conversions can be performed out of the box with quotes and splices.
But if `f` is a known lambda function, `'{ $f($x) }` will not beta-reduce the lambda in place.
This optimization is performed in a later phase of the compiler.
Not reducing the application immediately can simplify analysis of generated code.
Nevertheless, it is possible to beta-reduce the lambda in place using the `Expr.betaReduce` method.

```scala
def now[T: Type, U: Type](f: Expr[T => U]): Expr[T] => Expr[U] =
  (x: Expr[T]) => Expr.betaReduce('{ $f($x) })
```

The `betaReduce` method will beta-reduce the outermost application of the expression if possible (regardless of arity).
If it is not possible to beta-reduce the expression, then it will return the original expression.

## Staged Constructors
To create new class instances in a later stage, we can create them using factory methods (usually `apply` methods of an `object`), or we can instantiate them with a `new`.
For example, we can write `Some(1)` or `new Some(1)`, creating the same value.
In Scala 3, using the factory method call notation will fall back to a `new` if no `apply` method is found.
We follow the usual staging rules when calling a factory method.
Similarly, when we use a `new C`, the constructor of `C` is implicitly called, which also follows the usual staging rules.
Therefore for an arbitrary known class `C`, we can use both `'{ C(...) }` or `'{ new C(...) }` as constructors.

## Staged Classes
Quoted code can contain any valid expression including local class definitions.
This allows the creation of new classes with specialized implementations.
For example, we can implement a new version of `Runnable` that will perform some optimized operation.
```scala
def mkRunnable(x: Int)(using Quotes): Expr[Runnable] = '{
  class MyRunnable extends Runnable:
    def run(): Unit = ... // generate some custom code that uses `x`
  new MyRunnable
}
```

The quoted class is a local class and its type cannot escape the enclosing quote.
The class must be used inside the quote or an instance of it can be returned using a known interface (`Runnable` in this case).

## Quote Pattern Matching

It is sometimes necessary to analyze the structure of the code or decompose the code into its sub-expressions.
A classic example is an embedded DSL, where a macro knows a set of definitions that it can reinterpret while compiling the code (for instance, to perform optimizations).
In the following example, we extend our previous implementation of `powCode` to look into `x` to perform further optimizations.

```scala
def fusedPowCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
  x match
    case '{ power($y, $m) } => // we have (y^m)^n
      fusedPowCode(y, '{ $n * $m }) // generate code for y^(n*m)
    case _ =>
      '{ power($x, $n) }
```


#### Sub-patterns

In quoted patterns, the `$` binds the sub-expression to an expression `Expr` that can be used in that `case` branch.
The contents of `${..}` in a quote pattern are regular Scala patterns.
For example, we can use the `Expr(_)` pattern within the `${..}` to only match if it is a known value and extract it.

```scala
def fusedUnrolledPowCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
  x match
    case '{ power($y, ${Expr(m)}) } => // we have (y^m)^n
      fusedUnrolledPowCode(y, n * m) // generate code for y * ... * y
    case _ =>                        //                  ( n*m times )
      unrolledPowerCode(x, n)
```

These value extraction sub-patterns can be polymorphic using an instance of `FromExpr`.
In the following example, we show the implementation of `OptionFromExpr` which internally uses the `FromExpr[T]` to extract the value using the `Expr(x)` pattern.

```scala
given OptionFromExpr: [T: {Type, FromExpr}] => FromExpr[Option[T]]:
  def unapply(x: Expr[Option[T]])(using Quotes): Option[Option[T]] =
    x match
      case '{ Some( ${Expr(x)} ) } => Some(Some(x))
      case '{ None } => Some(None)
      case _ => None
```



#### Closed patterns
Patterns may contain two kinds of references: global references such as the call to the `power` method in `'{ power(...) }`, or references to bindings defined in the pattern such as `x` in `case '{ (x: Int) => x }`.
When extracting an expression from a quote, we need to ensure that we do not extrude any variable from the scope where it is defined.

```scala
'{ (x: Int) => x + 1 } match
  case '{ (y: Int) => $z } =>
    // should not match, otherwise: z = '{ x + 1 }
```

In this example, we see that the pattern should not match.
Otherwise, any use of the expression `z` would contain an unbound reference to `x`.
To avoid any such extrusion, we only match on a `${..}` if its expression is closed under the definitions within the pattern.
Therefore, the pattern will not match if the expression is not closed.

#### HOAS patterns
To allow extracting expressions that may contain extruded references we offer a _higher-order abstract syntax_ (HOAS) pattern `$f(y)` (or `$f(y1,...,yn)`).
This pattern will eta-expand the sub-expression with respect to `y` and bind it to `f`.
The lambda arguments will replace the variables that might have been extruded.

```scala
'{ ((x: Int) => x + 1).apply(2) } match
  case '{ ((y: Int) => $f(y): Int).apply($z: Int) } =>
    // f may contain references to `x` (replaced by `$y`)
    // f = '{ (y: Int) => $y + 1 }
    Expr.betaReduce('{ $f($z)}) // generates '{ 2 + 1 }
```


A HOAS pattern `$x(y1,...,yn)` will only match the expression if it does not contain references to variables defined in the pattern that are not in the set `y1,...,yn`.
In other words, the pattern will match if the expression only contains references to variables defined in the pattern that are in `y1,...,yn`.
Note that the HOAS patterns `$x()` are semantically equivalent to closed patterns `$x`.


#### Type variables

Expressions may contain types that are not statically known.
For example, an `Expr[List[Int]]` may contain `list.map(_.toInt)` where `list` is a `List` of some type.
To cover all the possible cases we would need to explicitly match `list` on all possible types (`List[Int]`, `List[Int => Int]`, ...).
This is an infinite set of types and therefore pattern cases.
Even if we would know all possible types that a specific program could use, we may still end up with an unmanageable number of cases.
To overcome this, we introduce type variables in quoted patterns, which will match any type.

In the following example, we show how type variables `t` and `u` match all possible pairs of consecutive calls to `map` on lists.
In the quoted patterns, types named with lower cases are identified as type variables.
This follows the same notation as type variables used in normal patterns.
```scala
def fuseMapCode(x: Expr[List[Int]]): Expr[List[Int]] =
  x match
    case '{ ($ls: List[t]).map[u]($f).map[Int]($g) } =>
      '{ $ls.map($g.compose($f)) }
    ...

fuseMapCode('{ List(1.2).map(f).map(g) }) // '{ List(1.2).map(g.compose(f)) }
fuseMapCode('{ List('a').map(h).map(i) }) // '{ List('a').map(i.compose(h))  }
```
Variables `f` and `g` are inferred to be of type `Expr[t => u]` and `Expr[u => Int]` respectively.
Subsequently, we can infer `$g.compose($f)` to be of type `Expr[t => Int]` which is the type of the argument of `$ls.map(..)`.

Type variables are abstract types that will be erased; this implies that to reference them in the second quote we need a given `Type[t]` and `Type[u]`.
The quoted pattern will implicitly provide those given types.
At run-time, when the pattern matches, the type of `t` and `u` will be known, and the `Type[t]` and `Type[u]` will contain the precise types in the expression.

As `Expr` is covariant, the statically known type of the expression might not be the actual type.
Type variables can also be used to recover the precise type of the expression.
```scala
def let(x: Expr[Any])(using Quotes): Expr[Any] =
  x match
    case '{ $x: t } =>
      '{ val y: t = $x; y }

let('{1}) // will return a `Expr[Any]` that contains an `Expr[Int]]`
```

It is also possible to refer to the same type variable multiple times in a pattern.

```scala
  case '{ $x: (t, t) } =>
```

While we can define the type variable in the middle of the pattern, their normal form is to define them as a `type` with a lower case name at the start of the pattern.

```scala
  case '{ type t; $x: t } =>
```

This is a bit more verbose but has some expressivity advantages such as allowing to define bounds on the variables.

```scala
  case '{ type t >: List[Int] <: Seq[Int]; $x: t } =>
```


#### Type patterns
It is possible to only have a type and no expression of that type.
To be able to inspect a type, we introduce quoted type pattern `case '[..] =>`.
It works the same way as a quoted pattern but is restricted to contain a type.
Type variables can be used in quoted type patterns to extract a type.

```scala
def empty[T: Type](using Quotes): Expr[T] =
  Type.of[T] match
    case '[String] => '{ "" }
    case '[List[t]] => '{ List.empty[t] }
    case '[type t <: Option[Int]; List[t]] => '{ List.empty[t] }
    ...
```
`Type.of[T]` is used to summon the given instance of `Type[T]` in scope, it is equivalent to `summon[Type[T]]`.

It is possible to match against a higher-kinded type using appropriate type bounds on type variables.
```scala
def empty[K <: AnyKind : Type](using Quotes): Type[?] =
  Type.of[K] match
    case '[type f[X]; f] => Type.of[f]
    case '[type f[X <: Int, Y]; f] => Type.of[f]
    case '[type k <: AnyKind; k ] => Type.of[k]
```

#### Type testing and casting
It is important to note that instance checks and casts on `Expr`, such as `isInstanceOf[Expr[T]]` and `asInstanceOf[Expr[T]]`, will only check if the instance is of the class `Expr` but will not be able to check the `T` argument.
These cases will issue a warning at compile-time, but if they are ignored, they can result in unexpected behavior.

These operations can be supported correctly in the system.
For a simple type test it is possible to use the `isExprOf[T]` method of `Expr` to check if it is an instance of that type.
Similarly, it is possible to use `asExprOf[T]` to cast an expression to a given type.
These operations use a given `Type[T]` to work around type erasure.


## Sub-Expression Transformation

The system provides a mechanism to transform all sub-expressions of an expression.
This is useful when the sub-expressions we want to transform are deep in the expression.
It is also necessary if the expression contains sub-expressions that cannot be matched using quoted patterns (such as local class definitions).

```scala
trait ExprMap:
  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T]
  def transformChildren[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] =
    ...
```

Users can extend the `ExprMap` trait and implement the `transform` method.
This interface is flexible and can implement top-down, bottom-up, or other transformations.

```scala
object OptimizeIdentity extends ExprMap:
  def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] =
    transformChildren(e) match // bottom-up transformation
      case '{ identity($x) } => x
      case _ => e
```

The `transformChildren` method is implemented as a primitive that knows how to reach all the direct sub-expressions and calls `transform` on each one.
The type passed to `transform` is the expected type of this sub-expression in its expression.
For example while transforming `Some(1)` in `'{ val x: Option[Int] = Some(1); ...}` the type will be `Option[Int]` and not `Some[Int]`.
This implies that we can safely transform `Some(1)` into `None`.

## Staged Implicit Summoning
When summoning implicit arguments using `summon`, we will find the given instances in the current scope.
It is possible to use `summon` to get staged implicit arguments by explicitly staging them first.
In the following example, we can pass an implicit `Ordering[T]` in a macro as an `Expr[Ordering[T]]` to its implementation.
Then we can splice it and give it implicitly in the next stage.

```scala
inline def treeSetFor[T](using ord: Ordering[T]): Set[T] =
  ${ setExpr[T](using 'ord) }

def setExpr[T:Type](using ord: Expr[Ordering[T]])(using Quotes): Expr[Set[T]] =
  '{ given Ordering[T] = $ord; new TreeSet[T]() }
```

We pass it as an implicit `Expr[Ordering[T]]` because there might be intermediate methods that can pass it along implicitly.

An alternative is to summon implicit values in the scope where the macro is invoked.
Using the `Expr.summon` method we get an optional expression containing the implicit instance.
This provides the ability to search for implicit instances conditionally.

```scala
def summon[T: Type](using Quotes): Option[Expr[T]]
```

```scala
inline def setFor[T]: Set[T] =
  ${ setForExpr[T] }

def setForExpr[T: Type]()(using Quotes): Expr[Set[T]] =
  Expr.summon[Ordering[T]] match
    case Some(ord) =>
      '{ new TreeSet[T]()($ord) }
    case _ =>
      '{ new HashSet[T] }
```

## More details

* [Specification](./macros-spec.md)
* Scalable Metaprogramming in Scala 3[^1]


[^1]: [Scalable Metaprogramming in Scala 3](https://infoscience.epfl.ch/record/299370)
[^2]: [Semantics-preserving inlining for metaprogramming](https://dl.acm.org/doi/10.1145/3426426.3428486)
[^3]: Implemented in the Scala 3 Dotty project https://github.com/lampepfl/dotty. sbt library dependency `"org.scala-lang" %% "scala3-staging" % scalaVersion.value`
[^4]: Using the `-Xcheck-macros` compiler flag
