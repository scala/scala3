---
layout: doc-page
title: Common Issue Locations
redirectFrom: /docs/contributing/workflow/areas.html
---

Many issues are localised to small domains of the compiler and are self-contained,
here is a non-exhaustive list of such domains, and the files associated with them:

### Pretty Printing of Types and Trees

Objects in the compiler that inherit from [Showable] can be pretty printed.
The pretty-printing of objects is used in many places, from debug output,
to user-facing error messages and printing of trees after each phase.

Look in [RefinedPrinter] (or its parent class [PlainPrinter]) for the implementation of pretty printing.

### Content of Error Messages

You can find the definitions of most error messages in [messages] (with IDs
defined in [ErrorMessageID]). If the message is not defined there, try the
`-Ydebug-error` compiler flag, which will print a stack trace leading to the
production of the error, and the contents of the message.

### Compiler Generated Given Instances

If the issue lies in given instances provided by the compiler, such as `scala.reflect.ClassTag`,
`scala.deriving.Mirror`, `scala.reflect.TypeTest`, `scala.CanEqual`, `scala.ValueOf`,
`scala.reflect.Manifest`, etc, look in [Synthesizer], which provides factories for
given instances.

### Compiler Generated Methods

Members can be generated for many classes, such as `equals` and `hashCode`
for case classes and value classes, and `ordinal` and `fromProduct` for Mirrors.
To change the implementation, see [SyntheticMembers].

### Code Completions
For suggestions to auto-complete method selections, see [Completion].

### Enum Desugaring
See [Desugar] and [DesugarEnums].

### Pattern Match Exhaustivity
See [Space].

### Metaprogramming

#### Quotes Reflection
See the [quoted runtime package][quotes-impl].

#### Inline match
See [Inliner].

#### Compiletime Ops Types
See `tryCompiletimeConstantFold` in [Types].

[Showable]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/printing/Showable.scala
[PlainPrinter]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/printing/PlainPrinter.scala
[RefinedPrinter]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/printing/RefinedPrinter.scala
[ErrorMessageID]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/reporting/ErrorMessageID.scala
[messages]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/reporting/messages.scala
[Synthesizer]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/typer/Synthesizer.scala
[SyntheticMembers]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/transform/SyntheticMembers.scala
[quotes-impl]: https://github.com/scala/scala3/tree/master/compiler/src/scala/quoted/runtime/impl
[Inliner]: https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/inlines/Inliner.scala
[Types]: https://github.com/scala/scala3/tree/master/compiler/src/dotty/tools/dotc/core/Types.scala
[Completion]: https://github.com/scala/scala3/tree/master/compiler/src/dotty/tools/dotc/interactive/Completion.scala
[DesugarEnums]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/ast/DesugarEnums.scala
[Desugar]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/ast/Desugar.scala
[Space]: https://github.com/scala/scala3/blob/master/compiler/src/dotty/tools/dotc/transform/patmat/Space.scala
