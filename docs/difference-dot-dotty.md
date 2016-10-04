Differences between DOT and Dotty
=================================
Dotty's type system is based on the [DOT calculus] but is much more rich,
this page attempts to list the differences between the two but is not
exhaustive. Note that both DOT and Dotty are in-progress work, so this page may
change.

## Features of Dotty's type system not present in DOT ##
* Type parameters, higher-kinded types and existential types. They are encoded
  using refinement types, see [Higher Kinded v2]
* Variance annotations (similiar to [SLS 4.5] Dotty additionally has
  variance annotations on type members).
* Singleton types like `x.type` (see [SLS 3.2.1])
  - The compiler has to be careful to detect cyclic type definitions involving
    singleton types like [tests/neg/cycles.scala]
* Type projections like `T#X` (see [SLS 3.2.2])
  - They are used to represent Java inner classes and to implement type lambdas
    in [Higher Kinded v2], the section [Status of#] describes their proposed
    meaning in Dotty.
  - See [#271] for a recent discussion on them.
  - They break subtyping transitivity:

    ```scala
    class A { type T }
    class B extends A { override type T = Int }
    class C extends A { override type T = String }
    ```
    `(B & C)#T` is an abstract type lower-bounded by `(Int | String)` and
    upper-bounded by `(Int & String)` (cf. the declaration lattice of DOT).
    This means that `Int <: (B & C)#T` and `(B & C)#T <: String`

  - Internally, path-dependent types like `p.T` are implemented as `p.type#T`,
    (cf. [Representation of types])
* Class types
  - They can only extend other class types, this excludes types like `X & Y` or
    `X { type T = Int }`, but includes parameterized types like `List[Int]`
    even though they're implemented using refined types (cf. [Higher Kinded v2])
  - Modeling them is not necessary because they're essentially refinement types
    with names, for example, we can replace

    ```scala
    class X
    class A extends X {
      type T = String
      def f: Int = 1
    }
    object O {
      val a: A = new A
    }
    ```
     by

    ```scala
    class X
    object O {
      type StructuralA = X { type T = String; def f: Int }
      val a: StructuralA = new X { type T = String; def f: Int = 1 }
    }
    ```
    Note that `A <: StructuralA`

* Type aliases like `type T = A`.
  - They are very similar to bounded abstract types like `type T >: A <: A`,
    but `class X extends T` only works with type aliases.

## Differences between DOT and Dotty for the types they have in common
The exact rules for what constitute a valid refinement type in Dotty are not
fixed yet, see [checkRefinementNonCyclic]

[DOT calculus]: http://lampwww.epfl.ch/~amin/dot/fool.pdf
[Higher Kinded v2]: https://github.com/lampepfl/dotty/blob/master/docs/internals/higher-kinded-v2.md
[SLS 4.5]: http://www.scala-lang.org/files/archive/spec/2.11/04-basic-declarations-and-definitions.html#variance-annotations
[SLS 3.2.1]: http://www.scala-lang.org/files/archive/spec/2.11/03-types.html#singleton-types
[tests/neg/cycles.scala]: https://github.com/lampepfl/dotty/blob/master/tests/neg/cycles.scala
[SLS 3.2.2]: http://www.scala-lang.org/files/archive/spec/2.11/03-types.html#type-projection
[Status of#]: https://github.com/lampepfl/dotty/blob/master/docs/HigherKinded-v2.md#status-of-
[#271]: https://github.com/lampepfl/dotty/pull/271#issuecomment-67288571
[Representation of types]: https://github.com/smarter/dotty/wiki/Type-System-Internals#representations-of-types
[checkRefinementNonCyclic]: https://github.com/lampepfl/dotty/blob/master/src/dotty/tools/dotc/typer/Checking.scala#L190
