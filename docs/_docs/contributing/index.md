---
layout: index
title: Contributing
---

This guide is intended to give new contributors the knowledge they need to
become productive and fix issues or implement new features in Scala 3. It
also documents the inner workings of the Scala 3 compiler, `dotc`.

### This is a living document

Keep in mind that the code for `dotc` is continually changing, so the ideas
discussed in this guide may fall out of date. This is a living document, so
please consider contributing to it on
[GitHub](https://github.com/scala/scala3/tree/main/docs/_docs/contributing) if
you notice anything out of date, or report any issues
[here](https://github.com/scala/scala3/issues).

### Get the most from This Guide

`dotc` is built with Scala 3, fully utilising its [new
features](https://docs.scala-lang.org/scala3/new-in-scala3.html). It is
recommended that you first have some familiarity with Scala 3 to get the most
out of this guide. You can learn more in the [language
reference](../reference/overview.md).

Many code snippets in this guide make use of shell commands (a line beginning
with `$`), and in this case a `bash` compatible shell is assumed. You may have
to look up how to translate commands to your shell.

### What is a Compiler?

Let's start at the beginning and first look at the question of "what is a
compiler?". A compiler is a program that takes as input text, representing a
program in one language and produces as output the same program, written in
another programming language.

#### The Scala Compiler

As an example, `dotc` takes text input, verifies that it is a valid Scala program
and then produces as output the same program, but written in Java bytecode, and optionally
in SJSIR when producing Scala.js output.
