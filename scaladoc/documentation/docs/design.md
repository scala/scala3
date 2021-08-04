# Design of scaladoc

Scaladoc is divided into 3 modules, that are:
- scaladoc
- scaladoc-js
- scaladoc-testcases

##### scaladoc

This module contains the core source code and assets (css, js) used for building Scaladoc.

##### scaladoc-js

This module contains Scala source code that is compiled to JavaScript. It is used by scaladoc for:
- searchbar
- social links
- snippets processor

##### scaladoc-testcases

Project that is used for manual testing as well as for end-to-end tests of Scaladoc.

## scaladoc

### Interface

scaladoc is intended to be use with sbt as well as from command line or from any other buildtool. The main entry point
- for sbt is [dotty.tools.dottydoc.Main](../../src/dotty/tools/dottydoc/Main.scala)
  This class is just for compatibility with old dottydoc and should be removed in the near future.
- for CLI is [dotty.tools.scaladoc.Main](../../src/dotty/tools/scaladoc/Main.scala)

### The base pipeline

The base pipeline of processing the files is as follows:

1. Gathering code data from TASTY files.
2. Creation of Members
3. Apply transformers
4. Rendering the members into HTML

Main processing starts from [Scaladoc](../../src/dotty/tools/scaladoc/Scaladoc.scala) class. This class is reponsible for checking
input settings and setting up [DocContext](../../src/dotty/tools/scaladoc/DocContext.scala) properly, which is wrapping `CompilerContext` with
all additional information required by Scaladoc.

Then everything goes to [ScalaModuleProvider](../../src/dotty/tools/scaladoc/ScalaModuleProvider.scala) where [Members](../../src/dotty/tools/scaladoc/api.scala) are created and then
transformations are applied.

Transformers are used to add additional data that could not be added at `Member`'s tree creation time.

Example transformers used are: [ImplicitMembersExtensionTransformer](../../src/dotty/tools/scaladoc/transformers/ImplicitMembersExtensionTransformer.scala),
[InheritanceInformationTransformer](../../src/dotty/tools/scaladoc/transformers/InheritanceInformationTransformer.scala),
[SealedMarksGraphTransformer](../../src/dotty/tools/scaladoc/transformers/SealedMarksGraphTransformer.scala).

The last step is serialization members into HTML pages. Everything that lies under package [dotty.tools.scaladoc.renderers](../../src/dotty/tools/scaladoc/renderers). It's worth noticing that whole template of page is coded as HTML building DSL like scalatags. The DocStrings are rendered by:
- if they are Markdown syntax comments - by flexmark dependency
- if they are WikiDoc syntax comments - by our own implementation

Unmentioned packages are:
#### [dotty.tools.scaladoc.parsers](../../src/dotty/tools/scaladoc/parsers)

Contains legacy parser for WikiDoc syntax CodeBlocks `{{{ }}}`.

#### [dotty.tools.scaladoc.site](../../src/dotty/tools/scaladoc/site)

Package responsible for generation static pages for blog and documentation.

#### [dotty.tools.scaladoc.tasty](../../src/dotty/tools/scaladoc/tasty)

Package havily used during `Members` creation. Contains preprocessors and parsers to extract `DocString` comments and
all semantic data from TASTY files.

Worth mentioning are:
- [TypesSupport](../../src/dotty/tools/scaladoc/tasty/TypesSupport.scala) - used to gather types that are human readable out of `Quotes` AST
- [ClassLikeSupport](../../src/dotty/tools/scaladoc/tasty/ClassLikeSupport.scala) - used to gather Members information out of `Quotes` AST

#### [dotty.tools.scaladoc.translators](../../src/dotty/tools/scaladoc/translators)

Utilities to create signatures for given Member.

#### [dotty.tools.scaladoc.util](../../src/dotty/tools/scaladoc/util)

Utilities for html, IO and JSONs.

### Important model classes

The core model class is called [Member](../../src/dotty/tools/scaladoc/api.scala) which aggregates all semantic data about some element of code.
It can represent Package, Class, Method etc. Members create Tree-like structure just like Classes do. Every `Member` has it's own identifier called
[DRI](../../src/dotty/tools/scaladoc/DRI.scala). `DRI` is easily serializable and unique. It's name is related to `DRI` model class from dokka,
on which scaladoc was firstly based. `DRI` lets you get URL of resource easily, doesn't matter if this is symbol from the same project or any external
dependency (given that correct external dependency is configured).

### Testing

In project we have some unit tests and end-to-end tests.
Unit tests are nothing special, just put them under `test` directory.
If you would like to creat some end-to-end tests you should visit [ScaladocTest](../../test/dotty/tools/scaladoc/ScaladocTest.scala) or
[BaseSignatureTest](../../test/dotty/tools/scaladoc/signatures/SignatureTest.scala).
The former is useful for any end-to-end tests, especially when used with JSoup html parsing.
The latter is used to compare expected/actual signatures of classes/methods etc.

To run tests just run `sbt scaladoc/test`

### Hack methods

Across the project there are multiple `hackXXX` methods around. The reasons why method is called like that can be various, however they probably do
some nasty casting, try-catching or any other procedure that was becuase of TASTY/dotty shortcomings or minor bugs. In the near future we should
revisit each of them and fix them on `dotty side or fix in any other way.

## scaladoc-js

### Interface

scaladoc-js is appended to js resources of published scaladoc. The main entry point is [Main](../../../scaladoc-js/src/Main.scala)
As a main method just append your javascripts transfomers that you find sufficient.
For now scaladoc-js handles Social Links and Searchbar, though there are plans to rewrite more js scipts like Diagram handler to Scala.js

## scaladoc-testcases

There is no general rule how to treat this package. If you want to use it as `easy to look up scaladoc output` module, just add your scala definitions
somewhere under `src` directory and run `sbt scaladoc/generateTestcasesDocumentation`. If you would like to use it as end-to-end test sourcecode input, check out [Testing](#testing) chapter.
