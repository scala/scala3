---
layout: doc-page
title: Scaladoc
---

Scaladoc is the documentation tool for [Scala
3](https://github.com/lampepfl/dotty). It uses the TastyInspector to access
definitions, which is an officially supported way to access Scala 3's
perspective of a codebase.

We're aiming to support all the features Scaladoc did, plus new and exciting ones such as:

- Markdown syntax!
- displaying project and API documentation together on one site!
- and more!

**Yes, this page was generated using scaladoc**

## Running the project

Use the following commands to generate documentation for this project and for Dotty, respectively:

```
sbt scaladoc/generateSelfDocumentation
sbt scaladoc/generateScalaDocumentation
```

To actually view the documentation, the easiest way is to run the following in the project root:

```
cd scaladoc/output
python3 -m http.server 8080
```

And afterwards point your browser to <http://localhost:8080/self> or
<http://localhost:8080/scala3> for this project and for Dotty documentation
respectively.

It's not strictly necessary to go through an HTTP server, but because of CORS
the documentation won't work completely if you don't.

## CLI and sbt Documentation

The preferred way to use scaladoc is calling it from sbt `Compile/doc` task or to use CLI interface provided inside `dotty/bin/scaladoc` bash script.
More information about specific scaladoc flags you can find inside [Usage docs](https://docs.scala-lang.org/scala3/guides/scaladoc/settings.html)

## Developing

For every PR named with prefix `scaladoc/`, we build documentation for scaladoc and Dotty. For example, for
PR `scaladoc/update-docs` you can find them at:

- <https://scala3doc.virtuslab.com/pr-update-docs/self/main/index.html>
- <https://scala3doc.virtuslab.com/pr-update-docs/scala3/main/index.html>
- <https://scala3doc.virtuslab.com/pr-update-docs/testcases/main/index.html>

Note that these correspond to the contents of `output` directory - that's
precisely what they are.

You can also find the result of building the same sites for latest `master` at:

- <https://scala3doc.virtuslab.com/master/self/main/index.html>
- <https://scala3doc.virtuslab.com/master/scala3/main/index.html>
- <https://scala3doc.virtuslab.com/master/testcases/main/index.html>

You can run the Scaladoc tool from the sbt prompt as a user would run it
with `scaladoc/run`, passing `-d <directory>` and some `.tasty` files as
input on the command line.

### Testing

From the sbt prompt, `scaladoc/test` runs the tests.

You can also run specific signature tests with `testOnly`,
for example `scaladoc/testOnly *scaladoc.signatures.MarkdownCode`.

Most tests rely on comparing signatures (of classes, methods, objects etc.) extracted from the generated documentation
to signatures found in source files (extracted using Scalameta). Such tests are defined using [SignatureTest](https://github.com/scala/scala3/blob/main/scaladoc/test/dotty/tools/scaladoc/signatures/SignatureTest.scala) class
and its subtypes (such as [TranslatableSignaturesTestCases](https://github.com/scala/scala3/blob/main/scaladoc/test/dotty/tools/scaladoc/signatures/TranslatableSignaturesTestCases.scala)). In this style of test, it's not necessary for expected output to be included, because the test is its own specification.

WARNING: As the classes mentioned above are likely to evolve, the description below might easily get out of date.
In case of any discrepancies rely on the source files instead.

`SignatureTest` requires that you specify the names of the files used to extract signatures,
the names of directories containing corresponding TASTY files
and the kinds of signatures from source files (corresponding to keywords used to declare them like `def`, `class`, `object` etc.)
whose presence in the generated documentation will be checked (other signatures, when missing, will be ignored).
The mentioned source files should be located directly inside the [scaladoc-testcases](https://github.com/scala/scala3/tree/main/scaladoc-testcases) directory
but the file names passed as parameters should contain neither this path prefix nor `.scala` suffix.

By default it's expected that all signatures from the source files will be present in the documentation
but not vice versa (because the documentation can contain also inherited signatures).
To validate that a signature present in the source does not exist in the documentation
(because they should be hidden from users) add `//unexpected` comment after the signature in the same line.
This will cause an error if a signature with the same name appears in the documentation
(even if some elements of the signature are slightly different - to avoid accidentally passing tests).
If the signature in the documentation is expected to slightly differ from how it's defined in the source code
you can add a `//expected: ` comment (also in the same line and followed by a space) followed by the expected signature.
Alternatively you can use `/*<-*/` and `/*->*/` as opening and closing parentheses for parts of a signature present in the source but undesired in the documentation (at least at the current stage of development), e.g.

```scala
def foo/*<-*/()/*->*/: Int
```

will make the expected signature be

```scala
def foo: Int
```

instead of

```scala
def foo(): Int
```

Because of the way how signatures in source are parsed, they're expected to span
until the end of a line (including comments except those special ones mentioned
above, which change the behaviour of tests) so if a definition contains an
implementation, it should be placed in a separate line, e.g.

```scala
def foo: Int
   = 1

class Bar
{
   //...
}
```

Otherwise the implementation would be treated as a part of the signature.

## FAQ

### Why use TASTy?

A documentation tool needs to access compiler information about the project - it
needs to list all definitions, resolve them by name, and query their members.
Tasty Reflect is the dedicated way in Scala 3 of accessing this information.

## Credits

- [Flatart](https://www.iconfinder.com/Flatart) - Gitter icon
