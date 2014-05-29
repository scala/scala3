Higher-Kinded Types in Dotty V2
===============================

This note outlines how we intend to represent higher-kinded types in
Dotty.  The principal idea is to collapse the four previously
disparate features of refinements, type parameters, existentials and
higher-kinded types into just one: refinements of type members. All
other features will be encoded using these refinements.

The complexity of type systems tends to grow exponentially with the
number of independent features, because there are an exponential
number of possible feature interactions. Consequently, a reduction
from 4 to 1 fundamental features achieves a dramatic reduction of
complexity. It also adds some nice usablilty improvements, notably in
the area of partial type application.

This is a second version of the scheme which differs in a key aspect
from the first one: Following Adriaan's idea, we use traits with type
members to model type lambdas and type applications. This is both more
general and more robust than the intersections with type constructor
traits that we had in the first version.

The duality
-----------

The core idea: A parameterized class such as

    class Map[K, V]

is treated as equivalent to a type with type members:

    class Map { type Map$K; type Map$V }

The type members are name-mangled (i.e. `Map$K`) so that they do not conflict with other
members or parameters named `K` or `V`.

A type-instance such as `Map[String, Int]` would then be treated as equivalent to

    Map { type Map$K = String; type Map$V = Int }

Named type parameters
---------------------

Type parameters can have unmangled names. This is achieved by adding the `type` keyword
to a type parameter declaration, analogous to how `val` indicates a named field. For instance,

    class Map[type K, type V]

is treated as equivalent to

    class Map { type K; type V }

The parameters are made visible as fields.

Wildcards
---------

A wildcard type such as `Map[_, Int]` is equivalent to

    Map { type Map$V = Int }

I.e. `_`'s omit parameters from being instantiated. Wildcard arguments
can have bounds. E.g.

    Map[_ <: AnyRef, Int]

is equivalent to

    Map { type Map$K <: AnyRef; type Map$V = Int }


Type parameters in the encodings
--------------------------------

The notion of type parameters makes sense even for encoded types,
which do not contain parameter lists in their syntax. Specifically,
the type parameters of a type are a sequence of type fields that
correspond to paraneters in the unencoded type. They are determined as
follows.

 - The type parameters of a class or trait type are those parameter fields declared in the class
   that are not yet instantiated, in the order they are given. Type parameter fields of parents
   are not considered.
 - The type parameters of an abstract type are the type parameters of its upper bound.
 - The type parameters of an alias type are the type parameters of its right hand side.
 - The type parameters of every other type is the empty sequence.

Partial applications
--------------------

The definition of type parameters in the previous section leads to a simple model of partial applications.
Consider for instance:

    type Histogram = Map[_, Int]

`Histogram` is a higher-kinded type that still has one type parameter.
`Histogram[String]`
would be a possible type instance, and it would be equivalent to `Map[String, Int]`.


Modelling polymorphic type declarations
---------------------------------------

The partial application scheme gives us a new -- and quite elegant --
way to do higher-kinded types. But how do we interprete the
poymorphic types that exist in current Scala?

More concretely, current Scala allows us to write parameterize type
definitions, abstract types, and type parameters. In the new scheme,
only classes (and traits) can have parameters and these are treated as
equivalent to type members. Type aliases and abstract types do not
allow the definition of type members so we have to interprete
polymorphic type aliases and abstract types specially.

Modelling polymorphic type aliases
----------------------------------

A polymorphic type alias such as

    type Pair[T] = (T, T)

is represented as a monomorphic type alias of a type lambda. Here's the expanded version of
the definition above:

    type Pair = Lambda1 { type Apply = (Arg1, Arg1) }

Here, `Lambda1` is a standard trait defined as follows:

    trait Lambda1[type Arg1, type Apply]

According to our definitions of type parameters `Lambda1` has two type parameters
and `Pair` has one.

There are `LambdaN` traits for higher arities as well. `Lambda` traits are special in that
they influence how type applications are expanded: If standard type applicatuon `T[X1, ..., Xn]`
leads to a subtype `S` of a type instance

      LambdaN { type Arg1 = T1; ...; type ArgN = Tn; type Apply ... }

where all argument fields `Arg1, ..., ArgN` are concretely defined
and the definition of the `Apply` field may be either abstract or concrete, then the application
is further expanded to `S # Apply`.

For instance, the type instance `Pair[String]` would be expanded to

    Lambda1 { type Arg1 = String; type Apply = (Arg1, Arg1) } # Apply

which turns out to be equal to `(String, String)`.

2nd Example: Consider the two aliases

    type RMap[K, V] = Map[V, K]]
    type RRMap[K, V] = RMap[V, K]

These expand as follows:

    type RMap  = Lambda2 { self1 => type Apply = Map[self1.Arg2, self1.Arg1] }
    type RRMap = Lambda2 { self2 => type Apply = RMap[self2.Arg2, self2.Arg1] }

Substituting the definition of `RMap` and expanding the type application gives:

    type RRMap = Lambda2 { self2 => type Apply =
                   Lambda2 { self1 => type Apply = Map[self1.Arg2, self1.Arg1] }
                           { type Arg1 = self2.Arg2; type Arg2 = self2.Arg1 } # Apply }

Substituting the definitions `self1.Arg{1,2}` gives:

    type RRMap = Lambda2 { self2 => type Apply =
                   Lambda2 { self1 => type Apply = Map[self2.Arg1, self2.Arg2] }
                           { type Arg1 = self2.Arg2; type Arg2 = self2.Arg1 } # Apply }

Simplifiying the `# Apply` selection gives:

    type RRMap = Lambda2 { self2 => type Apply = Map[self2.Arg1, self2.Arg2] }

This can be regarded as the eta-expanded version of `Map`. It has the same expansion as

    type IMap[K, V] = Map[K, V]


Modelling higher-kinded types
-----------------------------

The encoding of higher-kinded types uses again the `Lambda` traits to represent type constructors.
Consider the higher-kinded type declaration

    type Rep[T]

We expand this to

    type Rep <: Lambda1

The type parameters of `Rep` are the type parameters of its upper bound, so
`Rep` is a unary type constructor.

More generally, a higher-kinded type declaration

     type T[X1 >: S1 <: U1, ..., XN >: S1 <: UN] >: SR <: UR

is encoded as

     type T <: LambdaN { self =>
       type Arg1 >: s(S1) <: s(U1)
       ...
       type ArgN >: s(SN) <: s(UN)
       type Apply >: s(SR) <: s(UR)
     }

where `s` is the substitution `[XI := self.ArgI | I = 1,...,N]`.

If we instantiate `Rep` with a type argument, this is expanded as was explained before.

    Rep[String]

would expand to

    Rep { type Arg1 = String } # Apply

If we instantiate the higher-kinded type with a concrete type constructor (i.e. a parameterized
trait or class), we have to do one extra adaptation to make it work. The parameterized trait
or class has to be eta-expanded so that it comforms to the `Lambda` bound. For instance,

    type Rep = Set

would expand to

    type Rep = Lambda1 { type Apply = Set[Arg1] }

Or,

    type Rep = Map[String, _]

would expand to

    type Rep = Lambda1 { type Apply = Map[String, Arg1] }


Full example
------------

Consider the higher-kinded `Functor` type class

    class Functor[F[_]] {
       def map[A, B](f: A => B): F[A] => F[B]
    }

This would be represented as follows:

    class Functor[F <: Lambda1] {
       def map[A, B](f: A => B): F { type Arg1 = A } # Apply  =>  F { type Arg1 = B } # Apply
    }

The type `Functor[List]` would be represented as follows

    Functor {
       type F = Lambda1 { type Apply = List[Arg1] }
    }

Now, assume we have a value

    val ml: Functor[List]

Then `ml.map` would have type

    s(F { type Arg1 = A } # Apply  =>  F { type Arg1 = B } # Apply)

where `s` is the substitution of `[F := Lambda1 { type Apply = List[Arg1] }]`.
This gives:

    Lambda1 { type Apply = List[Arg1] } { type Arg1 = A } # Apply
     =>  Lambda1 { type Apply = List[Arg1] } { type Arg1 = B } # Apply

This type simplifies to:

     List[A] => List[B]

Status of #
-----------

In the scheme above we have silently assumed that `#` "does the right
thing", i.e. that the types are well-formed and we can collapse a type
alias with a `#` projection, thereby giving us a form of beta
reduction.

In Scala 2.x, this would not work, because `T#X` means `x.X forSome { val x: T }`.
Hence, two occurrences of `Rep[Int]` say, would not be recognized to be equal because the
existential would be opened each time afresh.

In pre-existentials Scala, this would not have worked either. There, `T#X` was a fundamental
type constructor, but was restricted to alias types or classes for both `T` and `X`.
Roughly, `#` was meant to encode Java's inner classes. In Java, given the classes

    class Outer { class Inner }
    class Sub1 extends Outer
    class Sub2 extends Outer

The types `Outer#Inner`, `Sub1#Inner` and `Sub2#Inner` would all exist and be
regarded as equal to each other. But if `Outer` had abstract type members this would
not work, since an abstract type member could be instantiated differently in `Sub1` and `Sub2`.
Assuming that `Sub1#Inner = Sub2#Inner` could then lead to a soundness hole. To avoid soundness
problems, the types in `X#Y` were restricted so that `Y` was (an alias of) a class type and
`X` was (an alias of) a class type with no abstract type members.

I believe we can drop this restriction and allow arbitrary type projects `X#Y` if we
are more careful with the subtyping rules. Specifically:

    A # X  <:  B # X

if either `A =:= B` (i.e. `A <: B` and `B <: A`), or the following three conditions hold:

  1. `X` is (an alias of) a class type,
  2. `B` is (an alias of) a class type without abstract type members.
  3. `A <: B`.

In essence, we allow abstract types `X`, `Y` in a projection `X#Y` but we prevent in this
case hiding conflicting type information in a subtype widening.

It would be good to study these rules formally, trying to verify their soundness.








