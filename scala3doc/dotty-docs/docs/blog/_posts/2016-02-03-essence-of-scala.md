---
layout: blog-page
title: The Essence of Scala
author: Martin Odersky
authorImg: images/martin.jpg
date: 2016-02-03
---

What do you get if you boil Scala on a slow flame and wait until all
incidental features evaporate and only the most concentrated essence
remains? After doing this for 8 years we believe we have the answer:
it's DOT, the calculus of dependent object types, that underlies Scala.

A [paper on DOT](http://infoscience.epfl.ch/record/215280) will be
presented in April at [Wadlerfest](http://events.inf.ed.ac.uk/wf2016),
an event celebrating Phil Wadler's 60th birthday. There's also a prior
technical report ([From F to DOT](http://arxiv.org/abs/1510.05216))
by Tiark Rompf and Nada Amin describing a slightly different version
of the calculus. Each paper describes a proof of type soundness that
has been machine-checked for correctness.

## The DOT calculus

A calculus is a kind of mini-language that is small enough to be
studied formally. Translated to Scala notation, the language covered
by DOT is described by the following abstract grammar:
```
Value       v  =  (x: T) => t            Function
                  new { x: T => ds }     Object

Definition  d  =  def a = t              Method definition
                  type A = T             Type

Term        t  =  v                      Value
                  x                      Variable
                  t1(t2)                 Application
                  t.a                    Selection
                  { val x = t1; t2 }     Local definition

Type        T  =  Any                    Top type
                  Nothing                Bottom type
                  x.A                    Selection
                  (x: T1) => T2          Function
                  { def a: T }           Method declaration
                  { type T >: T1 <: T2 } Type declaration
                  T1 & T2                Intersection
                  { x => T }             Recursion
```
The grammar uses several kinds of names:
```
x      for (immutable) variables
a      for (parameterless) methods
A      for types
```
The full calculus adds to this syntax formal _typing rules_ that
assign types `T` to terms `t` and formal _evaluation rules_ that
describe how a program is evaluated. The following _type soundness_
property was shown with a mechanized, (i.e. machine-checked) proof:

> If a term `t` has type `T`, and the evaluation of `t` terminates, then
  the result of the evaluation will be a value `v` of type `T`.

## Difficulties

Formulating the precise soundness theorem and proving it was unexpectedly hard,
because it uncovered some technical challenges that had not been
studied in depth before. In DOT - as well as in many programming languages -
you can have conflicting definitions. For instance, you might have an abstract
type declaration in a base class with two conflicting aliases in subclasses:
```scala
trait Base { type A }
trait Sub1 extends Base { type A = String }
trait Sub2 extends Base { type A = Int }
trait Bad extends Sub1 with Sub2
```
Now, if you combine `Sub1` and `Sub2` in trait `Bad` you get a conflict,
since the type `A` is supposed to be equal to both `String` and `Int`. If you do
not detect the conflict and assume the equalities at face value you
get `String = A = Int`, hence by transitivity `String = Int`! Once you
are that far, you can of course engineer all sorts of situations where
a program will typecheck but cause a wrong execution at runtime. In
other words, type soundness is violated.

Now, the problem is that one cannot always detect these
inconsistencies, at least not by a local analysis that does not need
to look at the whole program. What's worse, once you have an
inconsistent set of definitions you can use these definitions to
"prove" their own consistency - much like a mathematical theory that
assumes `true = false` can "prove" every proposition including its own
correctness.

The crucial reason why type soundness still holds is this: If one
compares `T` with an alias, one does so always relative to some _path_
`x` that refers to the object containing `T`.  So it's really `x.T =
Int`. Now, we can show that during evaluation every such path refers
to some object that was created with a `new`, and that, furthermore,
every such object has consistent type definitions. The tricky bit is
to carefully distinguish between the full typing rules, which allow
inconsistencies, and the typing rules arising from runtime values,
which do not.

## Why is This Important?

There are at least four reasons why insights obtained in the DOT
project are important.

 1. They give us a well-founded explanation of _nominal typing_.
    Nominal typing means that a type is distinguished from others
    simply by having a different name.
    For instance, given two trait definitions
    ```scala
    trait A extends AnyRef { def f: Int }
    trait B extends AnyRef { def f: Int }
    ```
    we consider `A` and `B` to be different types, even though both
    traits have the same parents and both define the same members.
    The opposite of
    nominal typing is structural typing, which treats types
    that have the same structure as being the same. Most programming
    languages are at least in part nominal whereas most formal type systems,
    including DOT, are structural. But the abstract types in DOT
    provide a way to express nominal types such as classes and traits.
    The Wadlerfest paper contains examples that show how
    one can express classes for standard types such as `Boolean` and `List` in DOT.

 2. They give us a stable basis on which we can study richer languages
    that resemble Scala more closely. For instance, we can encode
    type parameters as type members of objects in DOT. This encoding
    can give us a better understanding of the interactions of
    subtyping and generics. It can explain why variance rules
    are the way they are and what the precise typing rules for
    wildcard parameters `[_ <: T]`, `[_ >: T]` should be.

 3. DOT also provides a blueprint for Scala compilation. The new Scala
    compiler _Dotty_ has internal data structures that closely resemble DOT.
    In particular, type parameters are immediately mapped to type members,
    in the way we propose to encode them also in the calculus.

 4. Finally, the proof principles explored in the DOT work give us guidelines
    to assess and treat other possible soundness issues. We now know much
    better what conditions must be fulfilled to ensure type soundness.
    This lets us put other constructs of the Scala language to the test,
    either to increase our confidence that they are indeed sound, or
    to show that they are unsound. In my next blog I will
    present some of the issues we have discovered through that exercise.
