---
layout: doc-page
title: "Vararg Patterns"
---

The syntax of vararg patterns has changed. In the new syntax one writes varargs in patterns exactly
like one writes them in expressions, using a `: _*` type annotation:

```scala
xs match
case List(1, 2, xs: _*) => println(xs)    // binds xs
case List(1, _ : _*) =>                   // wildcard pattern
```

The old syntax, which is shorter but less regular, is no longer supported.

```scala
/*!*/ case List(1, 2, xs @ _*)       // syntax error
/*!*/ case List(1, 2, _*) => ...     // syntax error
```

The change to the grammar is:

```diff
 SimplePattern ::=  ‘_’
                 |  varid
                 |  Literal
                 |  StableId
                 |  StableId ‘(’ [Patterns ‘)’
-                |  StableId ‘(’ [Patterns ‘,’] [varid ‘@’] ‘_’ ‘*’ ‘)’
+                |  StableId ‘(’ [Patterns ‘,’] (varid | ‘_’) ‘:’ ‘_’ ‘*’ ‘)’
                 |  ‘(’ [Patterns] ‘)’
                 |  XmlPattern
```

## Compatibility considerations

To enable smooth cross compilation between Scala 2 and Scala 3, Dotty will
accept both the old and the new syntax. Under the `-source 3.1` setting, an error
will be emitted when the old syntax is encountered. They will be enabled by
default in version 3.1 of the language.

