---
layout: blog-page
title: Scaling DOT to Scala - Soundness
author: Martin Odersky
authorImg: images/martin.jpg
date: 2016-02-17
---

In my [last
blog post](http://www.scala-lang.org/blog/2016/02/03/essence-of-scala.html)
I introduced DOT, a minimal calculus that underlies much of Scala.
DOT is much more than an academic exercise, because it gives us
guidelines on how to design a sound type system for full Scala.

## Recap: The Problem of Bad Bounds

As was argued in the previous blog post, the danger a path-dependent type
system like Scala's faces is inconsistent bounds or aliases. For
instance, you might have a type alias
```scala
type T = String
```
in scope in some part of the program, but in another part the same
type member `T` is known as
```scala
type T = Int
```
If you connect the two parts, you end up allowing assigning a `String`
to an `Int` and vice versa, which is unsound - it will crash at
runtime with a `ClassCastException`. The problem is that there
is no obvious, practical, compile time analysis for DOT or
Scala that ensures that all types have good bounds. Types can contain
abstract type members with bounds that can be refined elsewhere and
several independent refinements might lead together to a bad bound
problem.  Barring a whole program analysis there is no specific
point in the program where we can figure this out straightforwardly.

In DOT, the problem is resolved by insisting that every path prefix `p`
of a type `p.T` is at runtime a concrete value. That way, we only have
to check for good bounds when objects are _created_ with `new`, and
that check is easy: When objects are created, we know their class and
we can insist that all nested types in that class are aliases or
have consistent bounds. So far so good.

## Loopholes Caused by Scaling Up

But if we want to scale up the DOT result for full Scala, several
loopholes open up. These come all down to the fact that the prefix of
a type selection might _not_ be a value that's constructed with a
`new` at run time.  The loopholes can be classified into three
categories:

 1. The prefix value might be lazy, and never instantiated to anything, as in:
    ```scala
    lazy val p: S = p
    ... p.T ...
    ```
    Note that trying to access the lazy value `p` would result in an infinite loop. But using `p` in a type does not force its evaluation, so we might never evaluate `p`. Since `p` is not initialized with a `new`, bad bounds for `T` would go undetected.

 2. The prefix value might be initialized to `null`, as in
    ```scala
    val p: S = null
    ... p.T ...
    ```
    The problem here is similar to the first one. `p` is not initialized
    with a `new` so we know nothing about the bounds of `T`.

 3. The prefix might be a type `T` in a type projection `T # A`, where `T`
    is not associated with a runtime value.

We can in fact construct soundness issues in all of these cases. Look
at the discussion for issues [#50](https://github.com/lampepfl/dotty/issues/50)
and [#1050](https://github.com/lampepfl/dotty/issues/1050) in the
[Dotty](https://github.com/lampepfl/dotty/issues/1050) repository
on GitHub. All issues work fundamentally in the same way: Construct a type `S`
which has a type member `T` with bad bounds, say:

```scala
Any <: T <: Nothing
```

Then, use the left subtyping to turn an expression of type `Any` into
an expression of type `T` and use the right subtyping to turn that
expression into an expression of type `Nothing`:

```scala
def f(x: Any): p.T = x
def g(x: p.T): Nothing = x
```

Taken together, `g(f(x))` will convert every expression into an
expression of type `Nothing`. Since `Nothing` is a subtype of every
other type, this means you can convert an arbitrary expression to have
any type you choose. Such a feat is an impossible promise, of
course. The promise is usually broken at run-time by failing with a
`ClassCastException`.

## Plugging the Loopholes

To get back to soundness we need to plug the loopholes. Some of the
necessary measures are taken in pull request [#1051](https://github.com/lampepfl/dotty/issues/1051).
That pull request

 - tightens the rules for overrides of lazy values: lazy values
   cannot override or implement non-lazy values,
 - tightens the rules which lazy values can appear in paths: they
   must be final and must have concrete types with known consistent bounds,
 - allows type projections `T # A` only if `T` is a concrete type
   with known consistent bounds.

It looks like this is sufficient to plug soundness problems (1) and
(3). To plug (2), we need to make the type system track nullability in
more detail than we do it now. Nullability tracking is a nice feature
in its own right, but now we have an added incentive for implementing
it: it would help to ensure type soundness.

There's one sub-case of nullability checking which is much harder to do
than the others. An object reference `x.f` might be `null` at run time
because the field `f` is not yet initialized. This can lead to a
soundness problem, but in a more roundabout way than the other issues
we have identified. In fact, Scala guarantees that in a program that
runs to completion without aborting, every field will eventually be
initialized, so every non-null field will have good bounds. Therefore,
the only way an initialized field `f` could cause a soundness problem
is if the program in question would never get to initialize `f`,
either because it goes into an infinite loop or because it aborts with
an exception or `System.exit` call before reaching the initialization
point of `f`. It's a valid question whether type soundness guarantees
should extend to this class of "strange" programs. We might want to
draw the line here and resort to runtime checks or exclude "strange"
programs from any soundness guarantees we can give. The research community
has coined the term [soundiness](http://soundiness.org/) for
this kind of approach and has [advocated](http://cacm.acm.org/magazines/2015/2/182650-in-defense-of-soundiness/fulltext) for it.

The necessary restrictions on type projection `T # A` are problematic
because they invalidate some idioms in type-level programming. For
instance, the cute trick of making Scala's type system Turing complete
by having it [simulate SK
combinators](https://michid.wordpress.com/2010/01/29/scala-type-level-encoding-of-the-ski-calculus/)
would no longer work since that one relies on unrestricted type
projections. The same holds for some of the encodings of type-level
arithmetic.

To ease the transition, we will continue for a while to allow unrestricted type
projections under a flag, even though they are potentially
unsound. In the current Dotty compiler, that flag is a language import
`-language:Scala2Compat`, but it could be something different for other
compilers, e.g. `-unsafe`.  Maybe we can find rules that are less
restrictive than the ones we have now, and are still sound.  But one
aspect should be non-negotiable: Any fundamental deviations from the
principles laid down by DOT needs to be proven mechanically correct
just like DOT was. We have achieved a lot with the DOT proofs, so we
should make sure not to back-slide. And if the experience of the past
10 years has taught us one thing, it is that the meta theory of type
systems has many more surprises in store than one might think. That's
why mechanical proofs are essential.
