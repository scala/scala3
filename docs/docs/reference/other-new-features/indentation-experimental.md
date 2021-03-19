---
layout: doc-page
title: "Fewer Braces"
---

By and large, the possible indentation regions coincide with those regions where braces `{...}` are also legal, no matter whether the braces enclose an expression or a set of definitions. There is one exception, though: Arguments to function can be enclosed in braces but they cannot be simply indented instead. Making indentation always significant for function arguments would be too restrictive and fragile.

To allow such arguments to be written without braces, a variant of the indentation scheme is implemented under language import
```scala
import language.experimental.fewerBraces
```
Alternatively, it can be enabled with command line option `-language:experimental.fewerBraces`.

This variant is more contentious and less stable than the rest of the significant indentation scheme. It allows to replace a function argument in braces by a `:` at the end of a line and indented code, similar to the convention for class bodies. It also allows to leave out braces around arguments that are multi-line function values.

## Using `:` At End Of Line


Similar to what is done for classes and objects, a `:` that follows a function reference at the end of a line means braces can be omitted for function arguments. Example:
```scala
times(10):
   println("ah")
   println("ha")
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

Braces can also be omitted around multiple line function value arguments. Examples
```scala
val xs = elems.map x =>
   val y = x - 1
   y * y
xs.foldLeft (x, y) =>
   x + y
```
Braces can be omitted if the lambda starts with a parameter list and `=>` or `=>?` at the end of one line and it has an indented body on the following lines.

## Syntax Changes

```
SimpleExpr  ::=  ...
              |  SimpleExpr : indent (CaseClauses | Block) outdent
              |  SimpleExpr FunParams (‘=>’ | ‘?=>’) indent Block outdent
```

Note that indented blocks after `:` or `=>` only work when following a simple expression, they are not allowed after an infix operator. So the following examples
would be incorrect:

```scala
   x + :      // error
      y

   f `andThen` y =>  // error
      y + 1
```

Note also that a lambda argument must have the `=>` at the end of a line for braces
to be optional. For instance, the following would also be incorrect:

```scala
  xs.map x => x + 1   // error: braces or parentheses are required
  xs.map(x => x + 1)  // ok
```
