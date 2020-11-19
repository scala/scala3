---
title: API Documentation
---

# {{ page.title }}

Scala3doc main feature is to create API documentation based on [doc comments](https://docs.scala-lang.org/style/scaladoc.html) known from scaladoc as well well as new syntax introduced in Scala 3.

## New syntax

### Links

The goal with the link syntax was to be as Scaladoc-compatible as possible,
while also making the links a bit more pleasant to type and read.
For the time being, Scala3doc mostly keeps Scaladoc's definition link syntax. We
did, however, implement some improvements to it:

1. `package` can be used as a prefix to reference the enclosing package
    Example:
    ```
    package utils
    class C {
      def foo = "foo".
    }
    /** See also [[package.C]]. */
    class D {
      def bar = "bar".
    }
    ```
    Links to the enclosing package in Scaladoc required mentioning the complete
    package name. Using the `package` keyword (similarly to how one would use
    `this` in expressions) helps make such links shorter.
1. `this` can be used as a prefix to reference the enclosing classlike
    Example:
    ```
    class C {
      def foo = "foo"
      /** This is not [[this.foo]], this is bar. */
      def bar = "bar"
    }
    ```
    Using a Scala keyword here helps make the links more familiar, as well as
    helps the links survive class name changes.
1. Backticks can be used to escape identifiers
    Example:
    ```
    def `([.abusive.])` = ???
    /** TODO: Figure out what [[`([.abusive.])`]] is. */
    def foo = `([.abusive.])`
    ```
    Scaladoc required backslash-escaping to reference such identifiers. Instead,
    Scala3doc allows using the familiar Scala backtick quotation.

### Why keep the Wiki syntax?

There are a few reasons why we've kept the Wiki syntax for documentation links
instead of reusing the Markdown syntax. Those are:

1. Nameless links in Markdown are ugly: `[](definition)` vs `[[definition]]`
    By far, most links in documentation are nameless. It should be obvious how to
    write them.
2. Local member lookup collides with URL fragments: `[](#field)` vs `[[#field]]`
3. Overload resolution collides with MD syntax: `[](meth(Int))` vs `[[meth(Int)]]`
4. Now that we have a parser for the link syntax, we can allow spaces inside (in
    Scaladoc one needed to slash-escape those), but that doesn't get recognized
    as a link in Markdown: `[](meth(Int, Float))` vs `[[meth(Int, Float)]]`

None of these make it completely impossible to use the standard Markdown link
syntax, but they make it much more awkward and ugly than it needs to be. On top
of that, Markdown link syntax doesn't even save any characters.