---
layout: doc-page
title: Core Data Structures
---

(The following is work in progress)

## Symbols and SymDenotations

 - why symbols are not enough: their contents change all the time
 - they change themselvesSo a `Symbol`
 - reference: string + sig


Dotc is different from most other compilers in that it is centered around the idea of
maintaining views of various artifacts associated with code. These views are indexed
by tne

A symbol refers to a definition in a source program. Traditionally,
 compilers store context-dependent data in a _symbol table_. The
 symbol then is the central reference to address context-dependent
 data. But for `scalac`'s requirements it turns out that symbols are
 both too little and too much for this task.

Too little: The attributes of a symbol depend on the phase. Examples:
Types are gradually simplified by several phases. Owners are changed
in phases `LambdaLift` (when methods are lifted out to an enclosing
class) and Flatten (when all classes are moved to top level). Names
are changed when private members need to be accessed from outside
their class (for instance from a nested class or a class implementing
a trait). So a functional compiler, a `Symbol` by itself met mean
much. Instead we are more interested in the attributes of a symbol at
a given phase.

`scalac` has a concept for "attributes of a symbol at

Too much: If a symbol is used to refer to a definition in another
compilation unit, we get problems for incremental recompilation. The
unit containing the symbol might be changed and recompiled, which
might mean that the definition referred to by the symbol is deleted or
changed. This leads to the problem of stale symbols that refer to
definitions that no longer exist in this form. Scala 2 compiler tried to
address this problem by _rebinding_ symbols appearing in certain cross
module references, but it turned out to be too difficult to do this
reliably for all kinds of references. Scala 3 compiler attacks the problem at
the root instead. The fundamental problem is that symbols are too
specific to serve as a cross-module reference in a system with
incremental compilation. They refer to a particular definition, but
that definition may not persist unchanged after an edit.

`scalac` uses instead a different approach: A cross module reference is
always type, either a `TermRef` or ` TypeRef`. A reference type contains
a prefix type and a name. The definition the type refers to is established
dynamically based on these fields.


a system where sources can be recompiled at any instance,

 the concept of a `Denotation`.

 Since definitions are transformed by phases,


The [Dotty project](https://github.com/lampepfl/dotty)
is a platform to develop new technology for Scala
tooling and to try out concepts of future Scala language versions.
Its compiler is a new design intended to reflect the
lessons we learned from work with the Scala compiler. A clean redesign
today will let us iterate faster with new ideas in the future.

Today we reached an important milestone: The Dotty compiler can
compile itself, and the compiled compiler can act as a drop-in for the
original one. This is what one calls a *bootstrap*.

## Why is this important?

The main reason is that this gives us a some validation of the
*trustworthiness* of the compiler itself. Compilers are complex beasts,
and many things can go wrong. By far the worst things that can go
wrong are bugs where incorrect code is produced. It's not fun debugging code that looks perfectly
fine, yet gets translated to something subtly wrong by the compiler.

Having the compiler compile itself is a good test to demonstrate that
the generated code has reached a certain level of quality. Not only is
a compiler a large program (44k lines in the case of dotty), it is
also one that exercises a large part of the language in quite
intricate ways. Moreover, bugs in the code of a compiler don't tend to
go unnoticed, precisely because every part of a compiler feeds into
other parts and all together are necessary to produce a correct
translation.

## Are We Done Yet?

Far from it! The compiler is still very rough. A lot more work is
needed to

 - make it more robust, in particular when analyzing incorrect programs,
 - improve error messages and warnings,
 - improve the efficiency of some of the generated code,
 - embed it in external tools such as sbt, REPL, IDEs,
 - remove restrictions on what Scala code can be compiled,
 - help in migrating Scala code that will have to be changed.

## What Are the Next Steps?

Over the coming weeks and months, we plan to work on the following topics:

 - Make snapshot releases.
 - Get the Scala standard library to compile.
 - Work on SBT integration of the compiler.
 - Work on IDE support.
 - Investigate the best way to obtaining a REPL.
 - Work on the build infrastructure.

If you want to get your hands dirty with any of this, now is a good moment to get involved!
To get started: <https://github.com/lampepfl/dotty>.

