---
layout: doc-page
title: "Changes in Implicit Resolution"
---

Implicit resolution uses a new algorithm which caches implicit results
more aggressively for performance. There are also some changes that
affect implicits on the language level.

 1. Types of implicit values and result types of implicit methods
    must be explicitly declared. Excepted are only values in local blocks
    where the type may still be inferred:

        class C {

          val ctx: Context = ...        // ok

          /*!*/ implicit val x = ...    // error: type must be given explicitly

          /*!*/ next(): Context = ...   // error: type must be given explicitly

          val y = {
            implicit val ctx = this.ctx // ok
            ...
          }

 2. Implicit parameters may not have singleton types.

        /*!*/ def f(implicit x: y.type) // error `y.type` not allowed as type of implicit

 3. Nesting is now taken into account for selecting an implicit.
    Consider for instance the following scenario:

        def f(implicit i: C) = {
          def g(implicit j: C) = {
            implicitly[C]
          }
        }

    This will now resolve the `implicitly` call to `j`, because `j` is nested
    more deeply than `i`. Previously, this would have resulted in an
    ambiguity error.

 4. The treatment of ambiguity errors has changed. If an ambiguity is encountered
    in some recursive step of an implicit search, the ambiguity is propagated to the caller.
    Example: Say you have the following definitions:

        class A
        class B extends C
        class C
        implicit def a1: A
        implicit def a2: A
        implicit def b(implicit a: A): B
        implicit def c: C

    and the query `implicitly[C]`.

    This query would now be classified as ambiguous. This makes sense, after all
    there are two possible solutions, `b(a1)` and `b(a2)`, neither of which is better
    than the other and both of which are better than the third solution, `c`.
    By contrast, Scala 2 would have rejected the search for `A` as
    ambiguous, and subsequently have classified the query `b(implicitly[A])` as a normal fail,
    which means that the alternative `c` would be chosen as solution!

    Scala 2's somewhat puzzling behavior with respect to ambiguity has been exploited to implement
    the analogue of a "negated" search in implicit resolution, where a query `Q1` fails if some
    other query `Q2` succeeds and `Q1` succeeds if `Q2` fails. With the new cleaned up behavior
    these techniques no longer work. But there is now a new special type `scala.implicits.Not`
    which implements negation directly. For any query type `Q`: `Not[Q]` succeeds if and only if
    the implicit search for `Q` fails.

 5. The treatment of divergence errors has also changed. A divergent implicit is
    treated as a normal failure, after which alternatives are still tried. This also makes
    sense: Encountering a divergent implicit means that we assume that no finite
    solution can be found on the given path, but another path can still be tried. By contrast
    most (but not all) divergence errors in Scala 2 would terminate the implicit
    search as a whole.


[//]: # (todo: expand with precise rules)
