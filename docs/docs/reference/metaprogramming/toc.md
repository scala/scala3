---
layout: doc-page
title: "Overview"
---

The following pages introduce the redesign of metaprogramming in Scala. They
introduce the following fundamental facilities:

1. [Inline](./inline.html) `inline` is a new soft-modifier that guarantees that
   a definition will be inlined at the point of use. The primary motivation
   behind inline is to reduce the overhead behind function calls and access to
   values. The expansion will be performed by the Scala compiler during the
   `Typer` compiler phase. However, as opposed to inline in other ecosystems,
   inlining is not merely a request to the compiler but for Scala it is a
   _command_. The reason is that inlining in Scala can driver other compile-time
   operations too, like inline pattern matching (enabling type-level
   programming), macros (enabling compile-time, generative, metaprogramming) and
   runtime code generation (multi-stage programming). In this section we
   describe up to inline pattern matching describing the basic constructs.

2. [Macros](./macros.html) Macros are built on two well-known fundamental
   operations: quotation and splicing.  Quotation is expressed as `'{...}` for
   expressions (both forms are equivalent) and as `'[...]` for types. Splicing
   is expressed as `${ ... }`. Whereas inlining is driven completely by the
   language level features of scala (pattern matching, inline vals and
   definitions), macros enable is synthesize/compute code at will treating code
   values as first class citizens and splicing them together independently.
   Here, we move towards _domain-specific_ metaprogramming.

3. [Staging](./staging.html) Macros can be seen as distinct phase while
   programming. You write your regular code that will be compiled according to
   the semantics of the language and the macro code that is going to be
   "compiled" or "generated" according the intented purpose of the programmer.
   Staging (or Multi-Stage Programming) can be seen as taking one step further
   this exact concept and can make code generation depent not only on static
   data but also on data available at _runtime_. This splits the evaluation of
   the program in many phases or ... stages, thus the "Multi-Stage" in the
   programming paradigm we say it supports.

4. [TASTy Reflection](./tasty-reflect.html) With TASTy reflection we can
   `unseal` fragments of code and analyze them with reflection over the TASTy
   format of the code.

5. [TASTy Inspection](./tasty-inspect.html) Up until now we described how we can
   expand, calculate at compile-time, or generate programs. The Scala compiler
   offers quarantees at the level of types that the generated programs cannot go
   wrong. With TASTy inspection we can load compiled files and analyze their
   typed AST structure according to the TASTy format.


