---
layout: doc-page
title: "Higher-Kinded Types in Dotty"
---

<aside class="warning">
    This page is out of date and preserved for posterity. Please see
    <a href="http://guillaume.martres.me/publications/dotty-hk.pdf">
    Implementing Higher-Kinded Types in Dotty</a> for a more up to date version
</aside>

Higher-Kinded Types in Dotty V2
===============================
This note outlines how we intend to represent higher-kinded types in Dotty.
The principal idea is to collapse the four previously disparate features of
refinements, type parameters, existentials and higher-kinded types into just
one: refinements of type members. All other features will be encoded using
these refinements.

The complexity of type systems tends to grow exponentially with the number of
independent features, because there are an exponential number of possible
feature interactions. Consequently, a reduction from 4 to 1 fundamental
features achieves a dramatic reduction of complexity. It also adds some nice
usablilty improvements, notably in the area of partial type application.

This is a second version of the scheme which differs in a key aspect from the
first one: Following Adriaan's idea, we use traits with type members to model
type lambdas and type applications. This is both more general and more robust
than the intersections with type constructor traits that we had in the first
version.

The duality
-----------
The core idea: A parameterized class such as

```scala
class Map[K, V]
```

is treated as equivalent to a type with type members:

```scala
class Map { type Map$K; type Map$V }
```

The type members are name-mangled (i.e. `Map$K`) so that they do not conflict
with other members or parameters named `K` or `V`.

A type-instance such as `Map[String, Int]` would then be treated as equivalent
to:

```scala
Map { type Map$K = String; type Map$V = Int }
```

Named type parameters
---------------------
Type parameters can have unmangled names. This is achieved by adding the `type`
keyword to a type parameter declaration, analogous to how `val` indicates a
named field. For instance,

```scala
class Map[type K, type V]
```

is treated as equivalent to

```scala
class Map { type K; type V }
```

The parameters are made visible as fields.

Wildcards
---------
A wildcard type such as `Map[_, Int]` is equivalent to:

```scala
Map { type Map$V = Int }
```

I.e. `_`'s omit parameters from being instantiated. Wildcard arguments can have
bounds. E.g.

```scala
Map[_ <: AnyRef, Int]
```

is equivalent to:

```scala
Map { type Map$K <: AnyRef; type Map$V = Int }
```

Type parameters in the encodings
--------------------------------
The notion of type parameters makes sense even for encoded types, which do not
contain parameter lists in their syntax. Specifically, the type parameters of a
type are a sequence of type fields that correspond to parameters in the
unencoded type. They are determined as follows.

* The type parameters of a class or trait type are those parameter fields declared in the class
  that are not yet instantiated, in the order they are given. Type parameter fields of parents
  are not considered.
* The type parameters of an abstract type are the type parameters of its upper bound.
* The type parameters of an alias type are the type parameters of its right hand side.
* The type parameters of every other type is the empty sequence.

Partial applications
--------------------
The definition of type parameters in the previous section leads to a simple
model of partial applications.  Consider for instance:

```scala
type Histogram = Map[_, Int]
```

`Histogram` is a higher-kinded type that still has one type parameter.
`Histogram[String]` would be a possible type instance, and it would be
equivalent to `Map[String, Int]`.


Modelling polymorphic type declarations
---------------------------------------
The partial application scheme gives us a new -- and quite elegant -- way to do
certain higher-kinded types. But how do we interprete the poymorphic types that
exist in current Scala?

More concretely, current Scala allows us to write parameterized type
definitions, abstract types, and type parameters. In the new scheme, only
classes (and traits) can have parameters and these are treated as equivalent to
type members. Type aliases and abstract types do not allow the definition of
parameterized types so we have to interprete polymorphic type aliases and
abstract types specially.

Modelling polymorphic type aliases: simple case
-----------------------------------------------
A polymorphic type alias such as:

```scala
type Pair[T] = Tuple2[T, T]
```

where `Tuple2` is declared as

```scala
class Tuple2[T1, T2] ...
```

is expanded to a monomorphic type alias like this:

```scala
type Pair = Tuple2 { type Tuple2$T2 = Tuple2$T1 }
```

More generally, each type parameter of the left-hand side must appear as a type
member of the right hand side type. Type members must appear in the same order
as their corresponding type parameters.  References to the type parameter are
then translated to references to the type member. The type member itself is
left uninstantiated.

This technique can expand most polymorphic type aliases appearing in Scala
codebases but not all of them. For instance, the following alias cannot be
expanded, because the parameter type `T` is not a type member of the right-hand
side `List[List[T]]`.

```scala
type List2[T] = List[List[T]]
```

We scanned the Scala standard library for occurrences of polymorphic type
aliases and determined that only two occurrences could not be expanded.  In
`io/Codec.scala`:

```scala
type Configure[T] = (T => T, Boolean)
```

And in `collection/immutable/HashMap.scala`:

```scala
private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)
```

For these cases, we use a fall-back scheme that models a parameterized alias as
a `Lambda` type.

Modelling polymorphic type aliases: general case
------------------------------------------------
A polymorphic type alias such as:

```scala
type List2D[T] = List[List[T]]
```

is represented as a monomorphic type alias of a type lambda. Here's the
expanded version of the definition above:

```scala
type List2D = Lambda$I { type Apply = List[List[$hkArg$0]] }
```

Here, `Lambda$I` is a standard trait defined as follows:

```scala
trait Lambda$I[type $hkArg$0] { type +Apply }
```

The `I` suffix of the `Lambda` trait indicates that it has one invariant type
parameter (named $hkArg$0).  Other suffixes are `P` for covariant type
parameters, and `N` for contravariant type parameters. Lambda traits can have
more than one type parameter. For instance, here is a trait with contravariant
and covariant type parameters:

```scala
trait Lambda$NP[type -$hkArg$0, +$hkArg$1] { type +Apply } extends Lambda$IP with Lambda$NI
```

Aside: the `+` prefix in front of `Apply` indicates that `Apply` is a covariant
type field. Dotty admits variance annotations on type members.

The definition of `Lambda$NP` shows that `Lambda` traits form a subtyping
hierarchy: Traits which have covariant or contravariant type parameters are
subtypes of traits which don't. The supertraits of `Lambda$NP` would themselves
be written as follows.

```scala
trait Lambda$IP[type $hkArg$0, +$hkArg$1] { type +Apply } extends Lambda$II
trait Lambda$NI[type -$hkArg$0, $hkArg$1] { type +Apply } extends Lambda$II
trait Lambda$II[type $hkArg$0, $hkArg$1] { type +Apply }
```

`Lambda` traits are special in that they influence how type applications are
expanded: If the standard type application `T[X1, ..., Xn]` leads to a subtype
`S` of a type instance

```scala
LambdaXYZ { type Arg1 = T1; ...; type ArgN = Tn; type Apply ... }
```

where all argument fields `Arg1, ..., ArgN` are concretely defined and the
definition of the `Apply` field may be either abstract or concrete, then the
application is further expanded to `S # Apply`.

For instance, the type instance `List2D[String]` would be expanded to

```scala
Lambda$I { type $hkArg$0 = String; type Apply = List[List[String]] } # Apply
```

which in turn simplifies to `List[List[String]]`.

2nd Example: Consider the two aliases

```scala
type RMap[K, V] = Map[V, K]
type RRMap[K, V] = RMap[V, K]
```

These expand as follows:

```scala
type RMap  = Lambda$II { self1 => type Apply = Map[self1.$hkArg$1, self1.$hkArg$0] }
type RRMap = Lambda$II { self2 => type Apply = RMap[self2.$hkArg$1, self2.$hkArg$0] }
```

Substituting the definition of `RMap` and expanding the type application gives:

```scala
type RRMap = Lambda$II { self2 => type Apply =
               Lambda$II { self1 => type Apply = Map[self1.$hkArg$1, self1.$hkArg$0] }
                 { type $hkArg$0 = self2.$hkArg$1; type $hkArg$1 = self2.$hkArg$0 } # Apply }
```

Substituting the definitions for `self1.$hkArg${1,2}` gives:

```scala
type RRMap = Lambda$II { self2 => type Apply =
               Lambda$II { self1 => type Apply = Map[self2.$hkArg$0, self2.$hkArg$1] }
                  { type $hkArg$0 = self2.$hkArg$1; type $hkArg$1 = self2.$hkArg$0 } # Apply }
```

Simplifiying the `# Apply` selection gives:

```scala
type RRMap = Lambda$II { self2 => type Apply = Map[self2.$hkArg$0, self2.$hkArg$1] }
```

This can be regarded as the eta-expanded version of `Map`. It has the same expansion as

```scala
type IMap[K, V] = Map[K, V]
```

Modelling higher-kinded types
-----------------------------
The encoding of higher-kinded types uses again the `Lambda` traits to represent
type constructors. Consider the higher-kinded type declaration

```scala
type Rep[T]
```

We expand this to

```scala
type Rep <: Lambda$I
```

The type parameters of `Rep` are the type parameters of its upper bound, so
`Rep` is a unary type constructor.

More generally, a higher-kinded type declaration

```scala
type T[v1 X1 >: S1 <: U1, ..., vn XN >: SN <: UN] >: SR <: UR
```

is encoded as

```scala
type T <: LambdaV1...Vn { self =>
  type v1 $hkArg$0 >: s(S1) <: s(U1)
  ...
  type vn $hkArg$N >: s(SN) <: s(UN)
  type Apply >: s(SR) <: s(UR)
}
```

where `s` is the substitution `[XI := self.$hkArg$I | I = 1,...,N]`.

If we instantiate `Rep` with a type argument, this is expanded as was explained
before.

```scala
Rep[String]
```

would expand to

```scala
Rep { type $hkArg$0 = String } # Apply
```

If we instantiate the higher-kinded type with a concrete type constructor (i.e.
a parameterized trait or class), we have to do one extra adaptation to make it
work. The parameterized trait or class has to be eta-expanded so that it
comforms to the `Lambda` bound. For instance,

```scala
type Rep = Set
```

would expand to:

```scala
type Rep = Lambda1 { type Apply = Set[$hkArg$0] }
```

Or,

```scala
type Rep = Map[String, _]
```

would expand to

```scala
type Rep = Lambda1 { type Apply = Map[String, $hkArg$0] }
```

Full example
------------
Consider the higher-kinded `Functor` type class

```scala
class Functor[F[_]] {
   def map[A, B](f: A => B): F[A] => F[B]
}
```

This would be represented as follows:

```scala
class Functor[F <: Lambda1] {
   def map[A, B](f: A => B): F { type $hkArg$0 = A } # Apply  =>  F { type $hkArg$0 = B } # Apply
}
```

The type `Functor[List]` would be represented as follows

```scala
Functor {
   type F = Lambda1 { type Apply = List[$hkArg$0] }
}
```

Now, assume we have a value

```scala
val ml: Functor[List]
```

Then `ml.map` would have type

```scala
s(F { type $hkArg$0 = A } # Apply  =>  F { type $hkArg$0 = B } # Apply)
```

where `s` is the substitution of `[F := Lambda1 { type Apply = List[$hkArg$0] }]`.
This gives:

```scala
Lambda1 { type Apply = List[$hkArg$0] } { type $hkArg$0 = A } # Apply
 =>  Lambda1 { type Apply = List[$hkArg$0] } { type $hkArg$0 = B } # Apply
```

This type simplifies to:

```scala
List[A] => List[B]
```

Status of `#`
-------------
In the scheme above we have silently assumed that `#` "does the right thing",
i.e. that the types are well-formed and we can collapse a type alias with a `#`
projection, thereby giving us a form of beta reduction.

In Scala 2.x, this would not work, because `T#X` means `x.X forSome { val x: T
}`. Hence, two occurrences of `Rep[Int]` say, would not be recognized to be
equal because the existential would be opened each time afresh.

In pre-existentials Scala, this would not have worked either. There, `T#X` was
a fundamental type constructor, but was restricted to alias types or classes
for both `T` and `X`.  Roughly, `#` was meant to encode Java's inner classes.
In Java, given the classes

```scala
class Outer { class Inner }
class Sub1 extends Outer
class Sub2 extends Outer
```

The types `Outer#Inner`, `Sub1#Inner` and `Sub2#Inner` would all exist and be
regarded as equal to each other. But if `Outer` had abstract type members this
would not work, since an abstract type member could be instantiated differently
in `Sub1` and `Sub2`.  Assuming that `Sub1#Inner = Sub2#Inner` could then lead
to a soundness hole. To avoid soundness problems, the types in `X#Y` were
restricted so that `Y` was (an alias of) a class type and `X` was (an alias of)
a class type with no abstract type members.

I believe we can go back to regarding `T#X` as a fundamental type constructor,
the way it was done in pre-existential Scala, but with the following relaxed
restriction:

> In a type selection `T#x`, `T` is not allowed to have any abstract members different from `X`

This would typecheck the higher-kinded types examples, because they only
project with `# Apply` once all `$hkArg$` type members are fully instantiated.

It would be good to study this rule formally, trying to verify its soundness.
