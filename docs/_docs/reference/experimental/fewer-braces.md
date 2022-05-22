---
layout: doc-page
title: "Fewer Braces"
nightlyOf: https://docs.scala-lang.org/scala3/reference/experimental/fewer-braces.html
---

By and large, the possible indentation regions coincide with those regions where braces `{...}` are also legal, no matter whether the braces enclose an expression or a set of definitions. There is one exception, though: Arguments to function can be enclosed in braces but they cannot be simply indented instead. Making indentation always significant for function arguments would be too restrictive and fragile.

To allow such arguments to be written without braces, a variant of the indentation scheme is implemented under language import
```scala
import language.experimental.fewerBraces
```
Alternatively, it can be enabled with command line option `-language:experimental.fewerBraces`.

This variant is more contentious and less stable than the rest of the significant indentation scheme. It allows to replace a function argument in braces by a `:` at the end of a line and indented code, similar to the convention for class bodies. The `:` can
optionally be followed by a lambda.

## Using `:` At End Of Line


Similar to what is done for classes and objects, a `:` that follows a function reference at the end of a line means braces can be omitted for function arguments. Example:
```scala
times(10):
  println("ah")
  println("ha")
```

The colon can also follow an infix operator:

```scala
credentials ++ :
  val file = Path.userHome / ".credentials"
  if file.exists
  then Seq(Credentials(file))
  else Seq()
```

Function calls that take multiple argument lists can also be handled this way:

```scala
val firstLine = files.get(fileName).fold:
    val fileNames = files.values
    s"""no file named $fileName found among
      |${values.mkString(\n)}""".stripMargin
  :
    f =>
      val lines = f.iterator.map(_.readLine)
      lines.mkString("\n)
```


## Lambda Arguments Without Braces

The `:` can optionally be followed by the parameter list of a function literal:
```scala
val xs = elems.map: x =>
  val y = x - 1
  y * y
xs.foldLeft (x, y) =>
  x + y
```
Braces can be omitted if the lambda starts with a parameter list and an arrow symbol `=>` or `?=>`. The arrow is followed by the body of the functional literal, which can be
either on the same line or as an indented block on the following lines. Example:
```scala
val xs = elems
  .map: x => x * x
  .foldLeft (x, y) => x = y
```

## Syntax Changes

As a lexical change, a `:` at the end of a line is now always treated as a
"colon at end of line" token.

The context free grammar changes as follows:
```
SimpleExpr       ::=  ...
                   |  SimpleExpr ‘:’ ColonArgument

                   |  SimpleExpr FunParams (‘=>’ | ‘?=>’) IndentedArgument
ColonArgument    ::=  indent CaseClauses | Block outdent
                    |  FunParams (‘=>’ | ‘?=>’) ColonArgBody
                    |  HkTypeParamClause ‘=>’ ColonArgBody
ColonArgBody     ::=  indent (CaseClauses | Block) outdent
                    |  <silent-indent> (CaseClauses | Block) outdent            --
```
The last line is understood as follows: If the token following a `=>` or `?=>` in a
`ColonArgument` is not an `indent`, then the parser inserts a silent indent token
and assumes the associated indentation region has maximal indentation width.
