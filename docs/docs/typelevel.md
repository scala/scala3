# Functional Typelevel Programming

This note is an exploration how we might combine staging with
typelevel metaprogramming in Dotty.

### State of the Art

Currently typelevel programming uses implicits as the basic mechanism,
resulting in a programming style much like Prolog. Amazing feats have
been achieved using this scheme, but things are certainly far from
ideal. In particular:

 - The logic programming style requires a shift of mindset compared to the
   usual functional programming style in Scala.
 - The ways to control implicit search are underdeveloped,
   leading to complicated schemes, requiring rule prioritization or other hacks.
 - Because of their conceptual complexity the resulting typelevel programs are often
   fragile.
 - Error diagnostics are usually very bad, making programming with implicits somewhat
   of a black art. Dotty has greatly improved error dignostics for recursive implicits,
   but the fundamental problem remains.

In the following we develop a system and methodology that lets us do
typelevel programming in a functional style. The system is based on
Dotty's symmetric meta programming system, adding some fairly modest
language extensions.

### Prequel: Conditions in Staged Code

As a perparatory step, we allow if-then-else in spliced code. Given an
if-then-else expression, the macro-interpreter interprets its
condition (which must be a compile-time constant) and chooses one of
the two branches depending on the condition's value. Consider the
following example mapping an integer value `n` to a peano number
represented as an instance of type `Nat`.

    trait Nat

    case object Z extends Nat
    case class S[N <: Nat] extends Nat

    inline def toNat(inline n: Int): Nat = ~{
      if n == 0 then 'Z
      else 'S[toNat(n - 1)]
    }

In order to avoid clutter we use quote syntax without mandatory
parentheses, e.g. `'Z` instead of `'(Z)`. (This assumes that symbol
literals are confined to Scala-2 mode.)

Note that the use of staging for `toNat` is recommended but not required. We could have
written

    inline def toNat(inline n: Int): Nat =
      if n == 0 then Z
      else S[toNat(n - 1)]

But then we would rely on inlining's nornalizations to simplify the
code to the branch that was taken. For more complicated examples this
would not always work, so we'd end up sometimes with code being
evaluated at runtime to compute a result that is already known at
compile-time. Since typelevel programming is based on information
known at compile-time, staging is a natural complement to it since it
distinguishes syntactically between computations done at compile time
and computations done at runtime. The syntactic distinctions also help
understanding the code better because they make clear what gets
computed when.

The ability to use conditionals in spliced code does not add anything
fundamental, since the same effect can be achieved by defining an
intermediate function that contains the conditional and is called in a
splice. By contrast, the next section does introduce a fundamental new
capability.

### First Step: Type Macros

In the previous example we would like to establish a stronger link
between the parameter `n` and the result type. For instance `toNat(3)`
should have static type `S[S[S[Z]]]` instead of `Nat`. To
get there, we allow macros that produce types. Just like an `inline
def` defines a macro yielding a value, an `inline type def` defines a macro
yielding a type.

Example:

    inline type def ToNat(inline n: Int) <: Nat = ~{
      if n == 0 then '[Z]
      else '[S[ToNat(n - 1)]]
    }

The right-hand side of an `inline type def` must be a splice
containing an expression of type `scala.quoted.Type[T]`, for some type
`T`.  A call to the macro in type position is then expanded to `T`.

A type macro may have a declared upper bound, e.g. `<: Nat` in the
example above. If an upper bound is given, the computed type `T` must
conform to it.  If no upper bound is given, `Any` is assumed. The
upper bound is used to type check recursive calls.

The `toNat` function can now be given a more precise type:

    inline def toNat(inline n: Int): ToNat(n) = ~{
      if n == 0 then 'Z
      else 'S[toNat(n - 1)]
    }

### Handling Recursion

A tricky aspect of recursive type functions is that their expansion
need not terminate. One could impose syntactic criteria to ensure
termination, but these interact badly with the implicit conditions
defined in the next section, since the compiler does in general not
know the relationship between the different types appearing in such a
definition.

Therefore, we propose to impose a (user-configurable) limit on the
number of expansion-steps or the time spent to expand in order to
prevent the compiler from going into an infinite loop or producing a
stack overflow, exactly in the same way it is done for normal `inline`
methods.

### Checking Correspondence Between Types and Terms

Another tricky aspect is how to ensure that the right-hand side of
`toNat` corresponds to its declared type. In the example above it is
easy to see that we use the same recursive decomposition of `n` in
both cases and that the result types of each case correspond. But to
prove this correspondence in general would require some sort of
symbolic evaluation and it is not yet clear what the details of that
would be.

But there is a simpler way: We can _delay_ checking the precise result
type of the body of an `macro` until the macro is inlined.  The
declared result type can be fully unfolded based on the static
information available at the inlining point. At that point the inlined
expansion of the macro is type-checked using the fully computed types.

That still begs the question how we are going to typecheck the
_definition_ of a macro. The idea is to approximate any computed
type at the definition that depends on unknown parameters by its upper
bound.

E.g., in the case of `toNat`, the type of the recursive call
`toNat(n - 1)` would be the upper bound `Nat` of the declared return type
`ToNat(n - 1)`. This gives `S[Nat]` as the type of the else
clause. The full type of the right hand side `Z | S[Nat]` would then
be checked against the upper approximation of the declared result type
`ToNat(n)`, which is again `Nat`. This succeeds, so the definition is deemed type-correct.


### Second Step: Nicer Syntax for Type Definitions

The syntax for `ToNat` was a bit clunky. This is no outlier - it turns out
that most inline type definitions use similar staging constructs. All
parameters tend to be inline, and the body is typically a conditional
expression which returns a quoted type in each branch. We can cut down
this boilerplate by introducing a shorthand form for parameterized
type definitions, which allows us to express `ToNat` as follows:

    type ToNat(n: Int) <: Nat =
      if n == 0 then Z
      else S[ToNat(n - 1)]

The short form expands precisely to the previous definition of `ToNat`.
To get from a short form type definition to a long form type macro,
the following rewritings are performed:

 - `type` is expanded to `inline type def`.
 - All value parameters get an `inline` modifier.
 - If the right hand side consists of a conditional, it is put in a spliced block `~{...}`.
 - The right hand sides of all conditionals are put in type quotes `'[...]`.

We expect that most computed types coming up in practice will use the
short form. The long form is kept around, to cater for more advanced
use cases, and to give a precise meaning to the short form.

### Third Step: Query Conditionals

What about defining the inverse of `ToNat`? This should be a function
that takes a _type_ of peano numbers and produces a compile-time
integer _value_. We can emulate the effect of such a function with
implicits but it's a bit roundabout since we would need several
overloaded methods each taking a different implicit parameter. A more
direct, closed formulation is possible if we allow implicit searches
as conditions in spliced code. Example:

    inline def toInt[N <: Nat]: Int = ~{
      case N =:= Z => '0
      case N =:= S[type N1] => '(1 + toInt[N1])
    }

A query conditional is written ` { <cases> }` where each case is of the form

    case <type-pattern> <guard> => <expr>

`<guard>` is either empty or of the form `if <expr>`. Some examples of legal cases are

    case Ord[X] => e1
    case X <:< List[Y] if n == 0 => e2
    case _ if n < 0 => e3

Query conditionals may only appear in spliced code. That sets them
apart from blocks with normal case clauses which define partial
functions and therefore can only appear in normal code.  Every query
conditional is evaluated by the macro interpreter. The macro
interpreter evaluates each case of a query conditional by performing
an implicit search for the given type pattern. If the search succeeds,
it continues with evaluating the guard, if there is one, followed by
the right-hand side, otherwise it continues with the next case.

Type patterns may bind type variables. For instance,
`N =:= S[type N1]` binds the type variable `N1`. Bound type variables are
instantiated by the implicit search. They are visible in the rest of
the case.

In general, a type name in an implicit condition is treated as a bound
variable if it is prefixed by `type`.

_Aside:_ We should introduce the same convention for types in normal
patterns. Currently type identifiers in patterns are treated as bound
if they start with a lower-case letter and as free otherwise. This
mirrors the convention for names, but is not very visible. Requiring
`type` prefixes instead would make programs clearer.

In last clause of the code above, the macro interpreter would perform
an implicit search for the type `N =:= S[type N1]` where `N` is the
actual type passed to `toInt` and `N1` is a type variable bound by the
search.  If the search succeeds, it returns one plus the result of
evaluating `toInt[N1]`. If the search fails, it results in compilation
failure.

The result of `toInt[S[S[Z]]]` would hence be the integer `1 + (1 +
0)`, which can be constant-folded to `2`. Alternatvely, we could drop
the use of quotes and rely on lifting to go from `Int` to
`Expr[Int]`. So the following would also be correct and would return a
constant without relying on constant folding:

    inline def toInt[N <: Nat]: Int = ~{
      case N =: Z => 0
      case N =:= S[type N1] => 1 + toInt[N1]
    }

### Example: HLists

Consider the standard definition of `HList`s:

    class HList

    case class HCons[X, Xs <: HList](hd: X, tl: Xs) extends HList
    case object HNil extends HList

A type-level length function on HLists can be written as follows:

    type Length[Xs <: HList] <: Nat = {
      case Xs =:= HNil => Z
      case Xs =:= HCons[type X, type Xs1] => S[Length[Xs1]]
    }

If we don't want to introduce the type variable `X`, which is unused, we could
also formulate the type pattern with `<:<` instead of `=:=`:

    type Length[Xs <: HList] <: Nat = {
      case Xs <:< HNil => Z
      case Xs <:< HCons[_, type Xs1] => S[Length[Xs1]]
    }

Here's a `Concat` type with associated `concat` method:

    type Concat[Xs <: HList, Ys <: HList] <: HList = {
      case Xs =:= HNil => Ys
      case Xs =:= HCons[type X, type Xs1] => HCons[X, Concat[Xs1, Ys]]
    }

    inline def concat[Xs <: HList, Ys <: HList](xs: Xs, ys: Ys): Concat[Xs, Ys] = ~ {
      case Xs =:= HNil => 'ys
      case Xs =:= HCons[type X, type Xs1] => 'HCons(xs.hd, concat(xs.tl, ys))
    }

### Typechecking Query Conditionals

Why does the last case of `concat` typecheck, given that `xs` is of
type `Xs`, which does not have `hd` or `tl` fields? Roughly, it
typechecks because `=:=` (and `<:<`) will inherit from
`ImplicitConverter` so in the presence of an implicit search result of
`T =:= U` or `T <:< U` we also get an implicit conversion from `T` to
`U`. But we still need to explain why a search for `Xs =:= HCons[type X, type Xs1]`
will succeed in the right hand side of the case. What happens in detail is this:

When type checking a query case, the type pattern is type-checked
according to the usual rules. Then the guard and right-hand side are type-checked
under the assumption that an implicit value of the type pattern is available.
For instance, when checking

    case T => E

`E` is typechecked in an environment that contains a definition like the following.

    inline implicit def $ev: T = implicitly[T]

Here, `$ev` stands for a compiler-defined fresh-name. The definition is declared `inline`
so that it can be used in quoted as well as unquoted code.

### Binding Implicit Condition Results

The implicit definition backing a query pattern can be bound to a name `x` using the syntax

    case x: T => ...

Using this device we can make the conversions in the `concat` function above explicit:

    inline def concat[Xs <: HList, Ys <: HList](xs: Xs, ys: Ys): Concat[Xs, Ys] = ~ {
      case Xs =:= HNil => 'ys
      case toCons: Xs =:= HCons[type X, type Xs1] => '(toCons(xs).hd :: concat(toCons(xs).tl, ys))
    }

### Tuple Syntax

With the projected identification of tuples and HLists, we can reformulate _length_ and _concat_ as follows:

    type Length[Xs <: Tuple] <: Nat = {
      case Xs =:= () => Z
      case Xs =:= (_, type Xs1) => S[Length[Xs1]]
    }

    type Concat[Xs <: Tuple, Ys <: Tuple] <: Tuple = {
      case Xs =:= () => Ys
      case Xs =:= (type X, type Xs1) => (X, Concat[Xs1, Ys])
    }

    inline def concat[Xs <: Tuple, Ys <: Tuple](xs: Xs, ys: Ys): Concat[Xs, Ys] = ~ {
      case Xs =:= () => 'Ys
      case Xs =:= (type X, type Xs1) => '(xs._1 :: concat(xs._2, ys))
    }

Here's another operation, _reverse_:

    type Reverse[Xs <: Tuple] <: Tuple = {
      case Xs =:= () => ()
      case Xs =:= (type X, type Xs1) => Concat[Reverse[Xs1], (X,())]
    }

    inline def reverse[Xs <: Tuple](xs: Xs): Reverse[Xs] = ~{
      case Xs =:= () => '()
      case Xs =:= (type X, type Xs1) => 'concat(reverse(xs._2), (xs._1,()))
    }

And here's _select_:

    type Select[Xs <: Tuple, N <: Nat] = {
      case Xs =:= (type X, type Xs1) =>
        {
          case N =:= Z => X
          case N =:= S[N1] => Select[Xs1, N1]
        }
    }

Note the two nested query conditionals, one for the tuple type `Xs`, the other for
the index `N`. We can also combine the queries in one conditional, as it is shown
in the corresponding `select` method:

    inline def select[Xs <: Tuple, N <: Nat](xs: Xs): Select[Xs, N] = ~{
      case Xs =:= (type X, type X1), N =:= Z => 'xs._1
      case Xs =:= (type X, type Xs1), N =:= S[N1] => 'select[Xs1, N1](xs._2)
    }

Multiple type patterns in a query conditionals are separated by commas and are checked
from left to right.

### Typelevel Booleans

We can define type-level Booleans analogously to Peano numbers:

    trait Bool

    implicit case object True extends Bool
    case object False extends Bool

(Note that `True` is defined to be an implicit value but `False` is not).

Here's a type function that returns the result of comparing two `Nat`
types `X` and `Y`, returning `True` iff the peano number defined by
`X` is smaller than the peano number defined by `Y` and `Falsesy` otherwise:

    type < [X <: Nat, Y <: Nat] <: Bool = {
      case X =:= Z, Y =:= S[type T1] => True
      case X =:= S[type X1], Y =:= S[type Y1] => Less[X1, Y1]
      case _ => False
    }

We can then use such comparisons as follows:

    type SafeSelect[Xs <: Tuple, N <: Nat](implicit ev: N < Length[Xs]) =
      Select[Xs, N]

The example demonstrates how one can get from the world of functional
programming to the world of logic programming and back. The `<` type
performs implicit queries and wraps the result in a function. The
`SafeSelect` type uses the result of the function as an implicit
evidence parameter. This works because `True` is implicit but `False`
is not.

### Exceptions Thrown By Macros

If none of the cases of a query conditional succeeds, the compiler will report
an error indicating that macro expansion failed. Macro implementations can
customize this by adding an else clause to the query conditional that throws
an exception indicating a more detailed error message. Example:

    inline def toInt[N <: Nat]: Int = ~ {
      case N =:= S[type N1] => '(1 + toInt[N1])
      case N =:= Z => '0
      case _ => throw new ExpansionError(s"underdefined type: $N is neither a `Z` nor a `S[T]`")
    }

An exception thrown by a macro is reported as a compile-time error at
the macro's expansion point. In the example above we would get
something like:

    toInt[Nat]
    ^^^^^^^^^^
    ExpansionError: "underdefined type: Nat is neither a `Z` nor a `S[T]`"

Here's another example:

    inline type def CheckedSelect[Xs <: Tuple, N <: Nat] = ~ {
      case N < Length[Xs] => '[Select[Xs, N]]
      case _ => throw new ExpansionError(s"index ${toInt(N)} out of bounds for $Xs"}
    }

The `CheckedSelect` method performs two count-downs of the number `N` - one to check
the length, the other to get to the element to select. We could reduce that to one
traversal and still keep the nice error message if we add `try` to the language supported
by the macro interpreter (this aspect of the proposal is more tentative than the others).
If we had `try`, `Select` could be reformulated as follows.

    class BadIndex extends Exception

    inline type def Select[Xs <: Tuple, N <: Nat] = ~ {
      try '[Select1[Xs, N]]
      catch {
        case ex: BadIndex =>
          throw new ExpansionError(s"index ${toInt(N)} out of bounds for $Xs")
      }
    }

    inline type def Select1[Xs <: Tuple, N <: Nat] = ~{
      case Xs =:= (type X, type Xs1) =>
        {
          case N =:= 'Z => '[X]
          case N =:= S[N1] => '[Select1[Xs1, N1]]
        }
      case _ => throw new BadIndex
    }

### Summary of Extensions

Compared to the basic meta programming system of Scala, we propose the
following language extensions:

 1. `implicit type def` macros which work like `inline def` macros, but map a right hand side
   expression of type `Type[T]` to the type `T` instead of mapping a right hand side
   of type `Expr[T]` to an expression of type `T`.

 2. Computed type definitions, which expand into `implicit type def` macros.

 3. Applications of computed types to arguments `T(args)`.

 4. Query conditinals that select a branch depending on the outcome of an implicit search
    for a type pattern. The type pattern may bind type variables and the result
    of the implicit search may also be bound to a variable. Query conditionals
    can only appear in spliced code.

(1-3) together constitute a logical complement to `inline def`
macros.  After all, if we can abstract over a splice that maps
expressions of type `Expr[T]` to expressions of type `T`, we should
also be able to abstract over a splice that maps expresssions of type
`Type[T]` to types `T`. (4) is in a sense the most interesting
addition since it gives us a way to use the success or failure of an
implicit search programmatically, thereby linking the world of logical
programming in implicit search with the world of functional
programming in the rest of Scala.

### Syntax Changes

Syntax changes are given relative to the [Dotty reference
grammar](../internal/syntax.md).

    PrefixExpr   ::=  [‘-’ | ‘+’ | ‘!’ | ‘~’ | ‘'’] SimpleExpr
                   |  ‘'’ ‘[’ Type ‘]’

    Def          ::=  ...
                   |  ‘type’ ‘def’ DefSig [‘<:’ Type] ‘=’ ‘~’ SimpleExpr
                   |  ‘type’ id [TypTypeParamClause] DefParamClauses [‘<:’ Type] ‘=’ Type

    SimpleExpr   ::=  ...
                   |  ‘{’ {Query ‘=>’ Block} ‘}’

    Query        ::=  case’ QueryPattern {"," QueryPattern} [ Guard ]
    QueryPattern ::=  [id ‘:’] TypePattern | ‘_’

    TypePattern  ::=  ... (productions as for Type, and: )
                   |  ‘type’ id TypeBounds

    SimpleType   ::=  ...
                   |  SimpleType ArgumentExprs
                   |  ‘{’ {Query ‘=>’ Type} ‘}’

### Implementation

From an implementation standpoint, the most important change is that
now types can be computed as macros, which means that splice expansion
must be done during typing, so it cannot be a separate phase anymore.
This style of "white box macros" poses implementation challenges, such
as how to keep compilers and IDEs working well in the presence of
resource hungry or mis-behaving macros. A mis-behaving macro could
slow down typechecking to unacceptable degrees, consume too much memory,
or pose a security risk.

Maybe the best way to mitigate the risks is to interpret all macro
code in the compiler. That way, one can impose limits on the number of
interpretaton steps per macro or prevent macros from calling external
libraries or doing system calls. Precedent for this is the D Language
implementation of meta programming, which seems to be well accepted.

A typer-based implementation of macros also strongly suggests that
splicing should be treated as syntax instead of being a user-defined
prefix operator. If splicing remains an operator, we know we have a
splice for `~t` only once `t` is typed, but then it is too late to set
up a proper splice context for `t`. Treating splices as syntax also
makes lifting work better since it gives us an expected type.


