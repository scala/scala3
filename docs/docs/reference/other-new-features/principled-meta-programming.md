---
layout: doc-page
title: "Principled Meta Programming"
---

Principled meta programming is a new framework for staging and for some
forms of macros. It is expressed as strongly and statically typed
code using two fundamental operations: quotations and splicing. A
novel aspect of the approach is that these two operations are
regulated by a phase consistency principle that treats quotes and
splices in exactly the same way.

## Overview

### Quotes and Splices

Principled meta programming is built on two well-known fundamental
operations: quotation and splicing.  Quotation is expressed as
`'{...}` for expressions (both forms are equivalent) and
as `'[...]` for types. Splicing is expressed as `${ ... }`.

For example, the code below presents an inline function `assert`
which calls at compile-time a method `assertImpl` with a boolean
expression tree as argument. `assertImpl` evaluates the expression and
prints it again in an error message if it evaluates to `false`.
```scala
    import scala.quoted._

    inline def assert(expr: => Boolean): Unit =
      ${ assertImpl('{ expr }) }

    def assertImpl(expr: Expr[Boolean]) = '{
      if !(${ expr }) then
        throw new AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
    }

    def showExpr(expr: Expr[Boolean]): Expr[String] =
      '{ "<some source code>" } // Better implementation later in this document
```
If `e` is an expression, then `'{e}` represent the typed
abstract syntax tree representing `e`. If `T` is a type, then `'[T]`
represents the type structure representing `T`.  The precise
definitions of "typed abstract syntax tree" or "type-structure" do not
matter for now, the terms are used only to give some
intuition. Conversely, `${e}` evaluates the expression `e`, which must
yield a typed abstract syntax tree or type structure, and embeds the
result as an expression (respectively, type) in the enclosing program.

Quotations can have spliced parts in them; in this case the embedded
splices are evaluated and embedded as part of the formation of the
quotation.

Quotes and splices can also be applied directly to identifiers. An identifier
`$x` starting with a `$` that appears inside a quoted expression or type is treated as a
splice `${x}`. Analogously, an quoted identifier 'x that appears inside a splice
is treated as a quote `'{x}`. See the Syntax section below for details.

Quotes and splices are duals of each other. For arbitrary
expressions `e` and types `T` we have:

    ${'{e}} = e
    '{${e}} = e
    ${'[T]} = T
    '[${T}] = T

### Types for Quotations

The type signatures of quotes and splices can be described using
two fundamental types:

  - `Expr[T]`: abstract syntax trees representing expressions of type `T`
  - `Type[T]`: type structures representing type `T`.

Quoting takes expressions of type `T` to expressions of type `Expr[T]`
and it takes types `T` to expressions of type `Type[T]`. Splicing
takes expressions of type `Expr[T]` to expressions of type `T` and it
takes expressions of type `Type[T]` to types `T`.

The two types can be defined in package `scala.quoted` as follows:
```scala
    package scala.quoted

    sealed abstract class Expr[T]
    sealed abstract class Type[T]
```
Both `Expr` and `Type` are abstract and sealed, so all constructors for
these types are provided by the system. One way to construct values of
these types is by quoting, the other is by type-specific lifting
operations that will be discussed later on.

### The Phase Consistency Principle

A fundamental *phase consistency principle* (PCP) regulates accesses
to free variables in quoted and spliced code:

 - _For any free variable reference `x`, the number of quoted scopes and the number of spliced scopes between the reference to `x` and the definition of `x` must be equal_.

Here, `this`-references count as free variables. On the other
hand, we assume that all imports are fully expanded and that `_root_` is
not a free variable. So references to global definitions are
allowed everywhere.

The phase consistency principle can be motivated as follows: First,
suppose the result of a program `P` is some quoted text `'{ ... x
... }` that refers to a free variable `x` in `P` This can be
represented only by referring to the original variable `x`. Hence, the
result of the program will need to persist the program state itself as
one of its parts. We don’t want to do this, hence this situation
should be made illegal. Dually, suppose a top-level part of a program
is a spliced text `${ ... x ... }` that refers to a free variable `x`
in `P`.  This would mean that we refer during _construction_ of `P` to
a value that is available only during _execution_ of `P`. This is of
course impossible and therefore needs to be ruled out.  Now, the
small-step evaluation of a program will reduce quotes and splices in
equal measure using the cancellation rules above. But it will neither
create nor remove quotes or splices individually. So the PCP ensures
that program elaboration will lead to neither of the two unwanted
situations described above.

In what concerns the range of features it covers, principled meta programming is
quite close to the MetaML family of languages. One difference is that MetaML does
not have an equivalent of the PCP - quoted code in MetaML _can_ access
variables in its immediately enclosing environment, with some
restrictions and caveats since such accesses involve serialization.
However, this does not constitute a fundamental gain in
expressiveness. Principled meta programming allows to define a `Liftable`
type-class which can implement such accesses within the confines of the
PCP. This is explained further in a later section.

## Details

### From `Expr`s to Functions and Back

The `Expr` companion object contains an implicit `AsFunctionN` (for 0 <= N < 23) conversion that turns a tree
describing a function into a function mapping trees to trees.
```scala
    object Expr {
      ...
      implied AsFunction1[T, U] for Conversion[Expr[T => U], Expr[T] => Expr[U]] ...
    }
```
This decorator gives `Expr` the `apply` operation of an applicative functor, where `Expr`s
over function types can be applied to `Expr` arguments. The definition
of `AsFunction1(f).apply(x)` is assumed to be functionally the same as
`'{($f)($x)}`, however it should optimize this call by returning the
result of beta-reducing `f(x)` if `f` is a known lambda expression.

The `AsFunction1` decorator distributes applications of `Expr` over function
arrows:
```scala
    AsFunction1(_).apply: Expr[S => T] => (Expr[S] => Expr[T])
```
Its dual, let’s call it `reflect`, can be defined as follows:
```scala
    def reflect[T, U](f: Expr[T] => Expr[U]): Expr[T => U] = '{
      (x: T) => ${ f('x) }
    }
```
Note how the fundamental phase consistency principle works in two
different directions here for `f` and `x`.  The reference to `f` is
legal because it is quoted, then spliced, whereas the reference to `x`
is legal because it is spliced, then quoted.

### Types and the PCP

In principle, The phase consistency principle applies to types as well
as for expressions. This might seem too restrictive. Indeed, the
definition of `reflect` above is not phase correct since there is a
quote but no splice between the parameter binding of `T` and its
usage. But the code can be made phase correct by adding a binding
of a `Type[T]` tag:
```scala
    def reflect[T, U](f: Expr[T] => Expr[U]) given (t: Type[T]): Expr[T => U] =
      '{ (x: $t) => ${ f('x) } }
```
In this version of `reflect`, the type of `x` is now the result of
splicing the `Type` value `t`. This operation _is_ splice correct -- there
is one quote and one splice between the use of `t` and its definition.

To avoid clutter, the Scala implementation tries to convert any phase-incorrect
reference to a type `T` to a type-splice, by rewriting `T` to `${ the[Type[T]] }`.
For instance, the user-level definition of `reflect`:
```scala
    def reflect[T: Type, U](f: Expr[T] => Expr[U]): Expr[T => U] =
      '{ (x: T) => ${ f('x) } }
```
would be rewritten to
```scala
    def reflect[T: Type, U](f: Expr[T] => Expr[U]): Expr[T => U] =
      '{ (x: ${ the[Type[T]] }) => ${ f('x) } }
```
The `the` query succeeds because there is an implied value of
type `Type[T]` available (namely the given parameter corresponding
to the context bound `: Type`), and the reference to that value is
phase-correct. If that was not the case, the phase inconsistency for
`T` would be reported as an error.

### Lifting Types

The previous section has shown that the metaprogramming framework has
to be able to take a type `T` and convert it to a type tree of type
`Type[T]` that can be reified. This means that all free variables of
the type tree refer to types and values defined in the current stage.

For a reference to a global class, this is easy: Just issue the fully
qualified name of the class. Members of reifiable types are handled by
just reifying the containing type together with the member name. But
what to do for references to type parameters or local type definitions
that are not defined in the current stage? Here, we cannot construct
the `Type[T]` tree directly, so we need to get it from a recursive
implicit search. For instance, to implement
```scala
    the[Type[List[T]]]
```
where `T` is not defined in the current stage, we construct the type constructor
of `List` applied to the splice of the result of searching for an implied instance for `Type[T]`:
```scala
    '[ List[ ${ the[Type[T]] } ] ]
```
This is exactly the algorithm that Scala 2 uses to search for type tags.
In fact Scala 2's type tag feature can be understood as a more ad-hoc version of
`quoted.Type`. As was the case for type tags, the implicit search for a `quoted.Type`
is handled by the compiler, using the algorithm sketched above.

### Example Expansion

Assume an `Array` class with an inline `map` method that forwards to a macro implementation.
```scala
    class Array[T] {
      inline def map[U](f: T => U): Array[U] = ${ Macros.mapImpl[T, U]('[U], 'this, 'f) }
    }
```
Here’s the definition of the `mapImpl` macro, which takes quoted types and expressions to a quoted expression:
```scala
    object Macros {

      def mapImpl[T, U](u: Type[U], arr: Expr[Array[T]], op: Expr[T => U]): Expr[Array[U]] = '{
        var i = 0
        val xs = $arr
        var len = xs.length
        val ys = new Array[$u](len)
        while (i < len) {
          ys(i) = ${ op('{ xs(i) }) }
          i += 1
        }
        ys
      }
    }
```
Here’s an application of `map` and how it rewrites to optimized code:
```scala
    genSeq[Int]().map(x => x + 1)
```
==> (inline)
```scala
    val _this: Seq[Int] = genSeq[Int]()
    val f: Int => Int = x => x + 1
    ${ _root_.Macros.mapImpl[Int, Int]('[Int], '_this, 'f) }
```
==> (splice)
```scala
    val _this: Seq[Int] = genSeq[Int]()
    val f: Int => Int = x => x + 1

    {
      var i = 0
      val xs = ${ '_this }
      var len = xs.length
      val ys = new Array[${ '[Int] }](len)
      while (i < len) {
        ys(i) = ${ ('f)('{ xs(i) }) }
        i += 1
      }
      ys
    }
```
==> (expand and splice inside quotes)
```scala
    val _this: Seq[Int] = genSeq[Int]()
    val f: Int => Int = x => x + 1

    {
      var i = 0
      val xs = _this
      var len = xs.length
      val ys = new Array[Int](len)
      while (i < len) {
        ys(i) = xs(i) + 1
        i += 1
      }
      ys
    }
```
==> (elim dead code)
```scala
    val _this: Seq[Int] = genSeq[Int]()

    {
      var i = 0
      val xs = _this
      var len = xs.length
      val ys = new Array[Int](len)
      while (i < len) {
        ys(i) = xs(i) + 1
        i += 1
      }
      ys
    }
```
### Relationship with Inline and Macros

Seen by itself, principled meta-programming looks more like a
framework for staging than one for compile-time meta programming with
macros. But combined with Dotty’s `inline` feature it can be turned into a
compile-time system.  The idea is that macro elaboration can be
understood as a combination of a macro library and a quoted
program. For instance, here’s the `assert` macro again together with a
program that calls `assert`.
```scala
    object Macros {

      inline def assert(expr: => Boolean): Unit =
        ${ assertImpl('expr) }

      def assertImpl(expr: Expr[Boolean]) =
        '{ if !($expr) then throw new AssertionError(s"failed assertion: ${$expr}") }
    }

    object App {
      val program = {
        val x = 1
        Macros.assert(x != 0)
      }
    }
```
Inlining the `assert` function would give the following program:
```scala
    val program = {
      val x = 1
      ${ Macros.assertImpl('{ x != 0) } }
    }
```
The example is only phase correct because Macros is a global value and
as such not subject to phase consistency checking. Conceptually that’s
a bit unsatisfactory. If the PCP is so fundamental, it should be
applicable without the global value exception. But in the example as
given this does not hold since both `assert` and `program` call
`assertImpl` with a splice but no quote.

However, one could argue that the example is really missing
an important aspect: The macro library has to be compiled in a phase
prior to the program using it, but in the code above, macro
and program are defined together. A more accurate view of
macros would be to have the user program be in a phase after the macro
definitions, reflecting the fact that macros have to be defined and
compiled before they are used. Hence, conceptually the program part
should be treated by the compiler as if it was quoted:
```scala
    val program = '{
      val x = 1
      ${ Macros.assertImpl('{ x != 0 }) }
    }
```
If `program` is treated as a quoted expression, the call to
`Macro.assertImpl` becomes phase correct even if macro library and
program are conceptualized as local definitions.

But what about the call from `assert` to `assertImpl`? Here, we need a
tweak of the typing rules. An inline function such as `assert` that
contains a splice operation outside an enclosing quote is called a
_macro_. Macros are supposed to be expanded in a subsequent phase,
i.e. in a quoted context. Therefore, they are also type checked as if
they were in a quoted context. For instance, the definition of
`assert` is typechecked as if it appeared inside quotes.  This makes
the call from `assert` to `assertImpl` phase-correct, even if we
assume that both definitions are local.

The `inline` modifier is used to declare a `val` that is
either a constant or is a parameter that will be a constant when instantiated. This
aspect is also important for macro expansion.  To illustrate this,
consider an implementation of the `power` function that makes use of a
statically known exponent:
```scala
    inline def power(inline n: Int, x: Double) = ${ powerCode(n, 'x) }

    private def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
      if (n == 0) '{ 1.0 }
      else if (n == 1) x
      else if (n % 2 == 0) '{ val y = $x * $x; ${ powerCode(n / 2, 'y) } }
      else '{ $x * ${ powerCode(n - 1, x) } }
```
The reference to `n` as an argument in `${ powerCode(n, 'x) }` is not
phase-consistent, since `n` appears in a splice without an enclosing
quote. Normally that would be a problem because it means that we need
the _value_ of `n` at compile time, which is not available for general
parameters. But since `n` is an inline parameter of a macro, we know
that at the macro’s expansion point `n` will be instantiated to a
constant, so the value of `n` will in fact be known at this
point. To reflect this, we loosen the phase consistency requirements
as follows:

 - If `x` is a inline value (or a inline parameter of an inline
   function) of type Boolean, Byte, Short, Int, Long, Float, Double,
   Char or String, it can be accessed in all contexts where the number
   of splices minus the number of quotes between use and definition
   is either 0 or 1.

### Relationship with Staging

The framework expresses at the same time compile-time meta-programming
and staging. The phase in which code is run is determined by the
difference between the number of splice scopes and quote scopes in
which it is embedded.

 - If there are more splices than quotes, the code is run at
   "compile-time" i.e. as a macro. In the general case, this means
   running an interpreter that evaluates the code, which is
   represented as a typed abstract syntax tree. The interpreter can
   fall back to reflective calls when evaluating an application of a
   previously compiled method.  If the splice excess is more than one,
   it would mean that a macro’s implementation code (as opposed to the
   code it expands to) invokes other macros. If macros are realized by
   interpretation, this would lead to towers of interpreters, where
   the first interpreter would itself interpret an interpreter code
   that possibly interprets another interpreter and so on.

 - If the number of splices equals the number of quotes, the code is
   compiled and run as usual.

 - If the number of quotes exceeds the number of splices, the code is
   staged. That is, it produces a typed abstract syntax tree or type
   structure at run-time. A quote excess of more than one corresponds
   to multi-staged programming.

Providing an interpreter for the full language is quite difficult, and
it is even more difficult to make that interpreter run efficiently. So
we currently impose the following restrictions on the use of splices.

 1. A top-level splice must appear in an inline method (turning that method
    into a macro)

 2. The splice must call a previously compiled
    method passing quoted arguments, constant arguments or inline arguments.

 3. Splices inside splices (but no intervening quotes) are not allowed.

 4. A macro method is effectively final and it may override no other method.

The framework as discussed so far allows code to be staged, i.e. be prepared
to be executed at a later stage. To run that code, there is another method
in class `Expr` called `run`. Note that `$` and `run` both map from `Expr[T]`
to `T` but only `$` is subject to the PCP, whereas `run` is just a normal method.
```scala
    sealed abstract class Expr[T] {
      def run  given (toolbox: Toolbox): T       // run staged code
      def show given (toolbox: Toolbox): String  // show staged code
    }
```

### Limitations to Splicing

Quotes and splices are duals as far as the PCP is concerned. But there is an additional
restriction that needs to be imposed on splices to guarantee soundness:
code in splices must be free of side effects. The restriction prevents code like this:
```scala
     var x: Expr[T]
     '{ (y: T) => ${ x = 'y; 1 } }
```
This code, if it was accepted, would "extrude" a reference to a quoted variable `y` from its scope.
This means we an subsequently access a variable outside the scope where it is defined, which is
likely problematic. The code is clearly phase consistent, so we cannot use PCP to
rule it out. Instead we postulate a future effect system that can guarantee that splices
are pure. In the absence of such a system we simply demand that spliced expressions are
pure by convention, and allow for undefined compiler behavior if they are not. This is analogous
to the status of pattern guards in Scala, which are also required, but not verified, to be pure.

There is also a problem with `run` in splices. Consider the following expression:
```scala
    '{ (x: Int) => ${ ('x).run; 1 } }
```
This is again phase correct, but will lead us into trouble. Indeed, evaluating the splice will reduce the
expression `('x).run` to `x`. But then the result
```scala
    '{ (x: Int) => ${ x; 1 } }
```
is no longer phase correct. To prevent this soundness hole it seems easiest to classify `run` as a side-effecting
operation. It would thus be prevented from appearing in splices. In a base language with side-effects we'd have to
do this anyway: Since `run` runs arbitrary code it can always produce a side effect if the code it runs produces one.

### The `Liftable` type-class

Consider the following implementation of a staged interpreter that implements
a compiler through staging.
```scala
    import scala.quoted._

    enum Exp {
      case Num(n: Int)
      case Plus(e1: Exp, e2: Exp)
      case Var(x: String)
      case Let(x: String, e: Exp, in: Exp)
    }
```
The interpreted language consists of numbers `Num`, addition `Plus`, and variables
`Var` which are bound by `Let`. Here are two sample expressions in the language:
```scala
    val exp = Plus(Plus(Num(2), Var("x")), Num(4))
    val letExp = Let("x", Num(3), exp)
```
Here’s a compiler that maps an expression given in the interpreted
language to quoted Scala code of type `Expr[Int]`.
The compiler takes an environment that maps variable names to Scala `Expr`s.
```scala
    import implied scala.quoted._

    def compile(e: Exp, env: Map[String, Expr[Int]]): Expr[Int] = e match {
      case Num(n) =>
        n.toExpr
      case Plus(e1, e2) =>
        '{ ${ compile(e1, env) } + ${ compile(e2, env) } }
      case Var(x) =>
        env(x)
      case Let(x, e, body) =>
        '{ val y = ${ compile(e, env) }; ${ compile(body, env + (x -> 'y)) } }
    }
```
Running `compile(letExp, Map())` would yield the following Scala code:
```scala
    '{ val y = 3; (2 + y) + 4 }
```
The body of the first clause, `case Num(n) => n.toExpr`, looks suspicious. `n`
is declared as an `Int`, yet it is converted to an `Expr[Int]` with `toExpr`.
Shouldn’t `n` be quoted? In fact this would not
work since replacing `n` by `'n` in the clause would not be phase
correct.

The `toExpr` extension method is defined in package `quoted`:
```scala
    package quoted

    implied LiftingOps {
      def (x: T) toExpr[T] given (ev: Liftable[T]): Expr[T] = ev.toExpr(x)
    }
```
The extension says that values of types implementing the `Liftable` type class can be
converted ("lifted") to `Expr` values using `toExpr`, provided an implied import
of `scala.quoted._` is in scope.

Dotty comes with implied instance definitions of `Liftable` for
several types including `Boolean`, `String`, and all primitive number
types. For example, `Int` values can be converted to `Expr[Int]`
values by wrapping the value in a `Literal` tree node. This makes use
of the underlying tree representation in the compiler for
efficiency. But the `Liftable` instances are nevertheless not "magic"
in the sense that they could all be defined in a user program without
knowing anything about the representation of `Expr` trees. For
instance, here is a possible instance of `Liftable[Boolean]`:
```scala
    implied for Liftable[Boolean] {
      def toExpr(b: Boolean) = if (b) '{ true } else '{ false }
    }
```
Once we can lift bits, we can work our way up. For instance, here is a
possible implementation of `Liftable[Int]` that does not use the underlying
tree machinery:
```scala
    implied for Liftable[Int] {
      def toExpr(n: Int): Expr[Int] = n match {
        case Int.MinValue    => '{ Int.MinValue }
        case _ if n < 0      => '{ - ${ toExpr(n) } }
        case 0               => '{ 0 }
        case _ if n % 2 == 0 => '{ ${ toExpr(n / 2) } * 2 }
        case _               => '{ ${ toExpr(n / 2) } * 2 + 1 }
      }
    }
```
Since `Liftable` is a type class, its instances can be conditional. For example,
a `List` is liftable if its element type is:
```scala
    implied [T: Liftable] for Liftable[List[T]] {
      def toExpr(xs: List[T]): Expr[List[T]] = xs match {
        case x :: xs1 => '{ ${ toExpr(x) } :: ${ toExpr(xs1) } }
        case Nil => '{ Nil: List[T] }
      }
    }
```
In the end, `Liftable` resembles very much a serialization
framework. Like the latter it can be derived systematically for all
collections, case classes and enums. Note also that the synthesis
of "type-tag" values of type `Type[T]` is essentially the type-level
analogue of lifting.

Using lifting, we can now give the missing definition of `showExpr` in the introductory example:
```scala
    def showExpr[T](expr: Expr[T]): Expr[String] = {
      val code = expr.show
      code.toExpr
    }
```
That is, the `showExpr` method converts its `Expr` argument to a string (`code`), and lifts
the result back to an `Expr[String]` using the `toExpr` wrapper.

**Note**: the `toExpr` extension method can be ommited by importing an implicit
conversion with `import scala.quoted.autolift._`. The programmer is able to
declutter slightly the code at the cost of readable _phase distinction_ between
stages.


## Implementation

### Syntax

Compared to the [Dotty reference grammar](../../internals/syntax.md)
there are the following syntax changes:

      SimpleExpr      ::=  ...
                        |  ‘'’ ‘{’ Block ‘}’
                        |  ‘'’ ‘[’ Type ‘]’
                        |  ‘$’ ‘{’ Block ‘}’
      SimpleType      ::=  ...
                        |  ‘$’ ‘{’ Block ‘}’

In addition, an identifier `$x` starting with a `$` that appears inside
a quoted expression or type is treated as a splice `${x}` and a quoted identifier
`'x` that appears inside a splice is treated as a quote `'{x}`

### Implementation in `dotc`

Quotes and splices are primitive forms in the generated abstract
syntax trees. They are eliminated in an expansion phase
`Staging`. This phase runs after typing and pickling.

Macro-expansion works outside-in. If the outermost scope is a splice,
the spliced AST will be evaluated in an interpreter. A call to a
previously compiled method can be implemented as a reflective call to
that method. With the restrictions on splices that are currently in
place that’s all that’s needed. We might allow more interpretation in
splices in the future, which would allow us to loosen the
restriction.  Quotes in spliced, interpreted code are kept as they
are, after splices nested in the quotes are expanded.

If the outermost scope is a quote, we need to generate code that
constructs the quoted tree at run-time. We implement this by
serializing the tree as a Tasty structure, which is stored
in a string literal. At runtime, an unpickler method is called to
deserialize the string into a tree.

Splices inside quoted code insert the spliced tree as is, after
expanding any quotes in the spliced code recursively.

## Formalization

The phase consistency principle can be formalized in a calculus that
extends simply-typed lambda calculus with quotes and splices.

### Syntax

The syntax of terms, values, and types is given as follows:

    Terms         t  ::=  x                 variable
                          (x: T) => t       lambda
                          t t               application
                          't                quote
                          $t                splice

    Values        v  ::=  (x: T) => t       lambda
                          'u                quote

    Simple terms  u  ::=  x  |  (x: T) => u  |  u u  |  't

    Types         T  ::=  A                 base type
                          T -> T            function type
                          expr T            quoted

Typing rules are formulated using a stack of environments
`Es`. Individual environments `E` consist as usual of variable
bindings `x: T`. Environments can be combined using the two
combinators `'` and `$`.

    Environment   E  ::=  ()                empty
                          E, x: T

    Env. stack    Es ::=  ()                empty
                          E                 simple
                          Es * Es           combined

    Separator     *  ::=  '
                          $

The two environment combinators are both associative with left and
right identity `()`.

### Operational semantics:

We define a small step reduction relation `-->` with the following rules:

                ((x: T) => t) v  -->  [x := v]t

                          ${'u}  -->  u

                             t1  -->  t2
                          -----------------
                          e[t1]  -->  e[t2]

The first rule is standard call-by-value beta-reduction. The second
rule says that splice and quotes cancel each other out. The third rule
is a context rule; it says that reduction is allowed in the hole `[ ]`
position of an evaluation context.  Evaluation contexts `e` and
splice evaluation context `e_s` are defined syntactically as follows:

    Eval context    e    ::=  [ ]  |  e t  |  v e  |  'e_s[${e}]
    Splice context  e_s  ::=  [ ]  |  (x: T) => e_s  |  e_s t  |  u e_s

### Typing rules

Typing judgments are of the form `Es |- t: T`. There are two
substructural rules which express the fact that quotes and splices
cancel each other out:

                          Es1 * Es2 |- t: T
                     ---------------------------
                     Es1 $ E1 ' E2 * Es2 |- t: T


                          Es1 * Es2 |- t: T
                     ---------------------------
                     Es1 ' E1 $ E2 * Es2 |- t: T

The lambda calculus fragment of the rules is standard, except that we
use a stack of environments. The rules only interact with the topmost
environment of the stack.

                              x: T in E
                            --------------
                            Es * E |- x: T


                        Es * E, x: T1 |- t: T2
                    -------------------------------
                    Es * E |- (x: T1) => t: T -> T2


                   Es |- t1: T2 -> T    Es |- t2: T2
                   ---------------------------------
                          Es |- t1 t2: T

The rules for quotes and splices map between `expr T` and `T` by trading `'` and `$` between
environments and terms.

                         Es $ () |- t: expr T
                         --------------------
                             Es |- $t: T


                           Es ' () |- t: T
                           ----------------
                           Es |- 't: expr T

The meta theory of a slightly simplified variant 2-stage variant of this calculus
is studied [separately](../simple-smp.md).

## Going Further

The meta-programming framework as presented and currently implemented is quite restrictive
in that it does not allow for the inspection of quoted expressions and
types. It’s possible to work around this by providing all necessary
information as normal, unquoted inline parameters. But we would gain
more flexibility by allowing for the inspection of quoted code with
pattern matching. This opens new possibilities. For instance, here is a
version of `power` that generates the multiplications directly if the
exponent is statically known and falls back to the dynamic
implementation of power otherwise.
```scala
    inline def power(n: Int, x: Double): Double = ${
      'n match {
        case Constant(n1) => powerCode(n1, 'x)
        case _ => '{ dynamicPower(n, x) }
      }
    }

    private def dynamicPower(n: Int, x: Double): Double =
      if (n == 0) 1.0
      else if (n % 2 == 0) dynamicPower(n / 2, x * x)
      else x * dynamicPower(n - 1, x)
```
This assumes a `Constant` extractor that maps tree nodes representing
constants to their values.

With the right extractors, the "AsFunction" conversion
that maps expressions over functions to functions over expressions can
be implemented in user code:
```scala
    implied AsFunction1[T, U] for Conversion[Expr[T => U], Expr[T] => Expr[U]] {
      def apply(f: Expr[T => U]): Expr[T] => Expr[U] =
       (x: Expr[T]) => f match {
         case Lambda(g) => g(x)
         case _ => '{ ($f)($x) }
       }
    }
```
This assumes an extractor
```scala
    object Lambda {
      def unapply[T, U](x: Expr[T => U]): Option[Expr[T] => Expr[U]]
    }
```
Once we allow inspection of code via extractors, it’s tempting to also
add constructors that create typed trees directly without going
through quotes. Most likely, those constructors would work over `Expr`
types which lack a known type argument. For instance, an `Apply`
constructor could be typed as follows:
```scala
    def Apply(fn: Expr[_], args: List[Expr[_]]): Expr[_]
```
This would allow constructing applications from lists of arguments
without having to match the arguments one-by-one with the
corresponding formal parameter types of the function. We then need "at
the end" a method to convert an `Expr[_]` to an `Expr[T]` where `T` is
given from the outside. E.g. if `code` yields a `Expr[_]`, then
`code.atType[T]` yields an `Expr[T]`. The `atType` method has to be
implemented as a primitive; it would check that the computed type
structure of `Expr` is a subtype of the type structure representing
`T`.

Before going down that route, we should evaluate in detail the tradeoffs it
presents.  Constructing trees that are only verified _a posteriori_
to be type correct loses a lot of guidance for constructing the right
trees.  So we should wait with this addition until we have more
use-cases that help us decide whether the loss in type-safety is worth
the gain in flexibility. In this context, it seems that deconstructing types is
less error-prone than deconstructing terms, so one might also
envisage a solution that allows the former but not the latter.

## Conclusion

Meta-programming has a reputation of being difficult and confusing.
But with explicit `Expr/Type` types and quotes and splices it can become
downright pleasant. A simple strategy first defines the underlying quoted or unquoted
values using `Expr` and `Type` and then inserts quotes and splices to make the types
line up. Phase consistency is at the same time a great guideline
where to insert a splice or a quote and a vital sanity check that
the result makes sense.
