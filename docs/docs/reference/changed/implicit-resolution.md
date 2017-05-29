---
layout: doc-page
title: "Changes in Implicit Resolution"
---

Implicit resolution uses a new algorithm which caches implicit results
more aggressively for perforance. There are also some changes that
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
    Consider for instance the following scenario

        def f(implicit i: C) = {
          def g(implicit j: C) = {
            implicitly[C]
          }
        }

    This will now resolve the `implicitly` call to `j`, because `j` is nested
    more deeply than `i`. Previously, this would have resulted in an
    ambiguity error.

[//] # todo: expand with precise rules


