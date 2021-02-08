# Scaladoc

Scaladoc  is the documentation tool for
[Scala 3](https://github.com/lampepfl/dotty), which is scheduled to become
Scala 3.  It uses the TastyInspector to access definitions,
which is an officially supported way to access Scala 3's perspective of a
codebase.

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

To actually view the documentation, the easiest way is to run the following in project root:

```
cd output
python3 -m http.server 8080
```

And afterwards point your browser to <http://localhost:8080/self> or
<http://localhost:8080/stdLib> for this project and for Dotty documentation
respectively.

It's not strictly necessary to go through an HTTP server, but because of CORS
the documentation won't work completely if you don't.

## CLI Documentation

CLI command for running our tool is in form: `sbt main -n <name> -o <output> -t <tasty-files> -cp <classpath> -s { <sources> } -d <documentation> ` where:

- `<name>`: name of module in generated documentation
- `<output>`: location where documentation should be created
- `<tasty-files>`: is list of dirs or jars that contains tasty files that should be documented
- `<classpath>`: classpath that was used to generate tasty files
- `<sources>`: links to source files of module that are used to link symbols on pages to their source file. They need to be supplied in form:
  `local_dir=remote_dir#line_suffix` e.g. `src/main/scala=https://github.com/lampepfl/scaladoc/tree/master/src/main/scala#L`
- `<documentation>`: directory of static documentation that you would like to render with API documentation.

## Developing

At least two of our contributors use [Metals](https://scalameta.org/metals/) to
work on the project.

For every PR, we build documentation for scaladoc and Dotty. For example, for
PR 123 you can find them at:

- <https://scala3doc.virtuslab.com/pr-123/self/main/index.html>
- <https://scala3doc.virtuslab.com/pr-123/scala3/main/index.html>
- <https://scala3doc.virtuslab.com/pr-123/testcases/main/index.html>

Note that these correspond to the contents of `output` directory - that's
precisely what they are.

You can also find the result of building the same sites for latest `master` at:

- <https://scala3doc.virtuslab.com/master/self/main/index.html>
- <https://scala3doc.virtuslab.com/master/scala3/main/index.html>
- <https://scala3doc.virtuslab.com/master/testcases/main/index.html>

### Testing

Most tests rely on comparing signatures (of classes, methods, objects etc.) extracted from the generated documentation
to signatures found in source files. Such tests are defined using [MultipleFileTest](test/dotty/tools/scala3doc/MultipleFileTest.scala) class
and its subtypes (such as [SingleFileTest](test/dotty/tools/scala3doc/SingleFileTest.scala))

WARNING: As the classes mentioned above are likely to evolve, the description below might easily get out of date.
In case of any discrepancies rely on the source files instead.

`MultipleFileTest` requires that you specify the names of the files used to extract signatures,
the names of directories containing corresponding TASTY files
and the kinds of signatures from source files (corresponding to keywords used to declare them like `def`, `class`, `object` etc.)
whose presence in the generated documentation will be checked (other signatures, when missing, will be ignored).
The mentioned source files should be located directly inside `src/main/scala/tests` directory
but the file names passed as parameters should contain neither this path prefix nor `.scala` suffix.
The TASTY folders are expected to be located in `target/${dottyVersion}/classes/tests` (after successful compilation of the project)
and similarly only their names relative to this path should be provided as tests' parameters.
For `SingleFileTest` the name of the source file and the TASTY folder are expected to be the same.

By default it's expected that all signatures from the source files will be present in the documentation
but not vice versa (because the documentation can contain also inherited signatures).
To validate that a signature present in the source does not exist in the documentation
(because they should be hidden from users) add `//unexpected` comment after the signature in the same line.
This will cause an error if a signature with the same name appears in the documentation
(even if some elements of the signature are slightly different - to avoid accidentally passing tests).
If the signature in the documentation is expected to slightly differ from how it's defined in the source code
you can add a `//expected: ` comment (also in the same line and followed by a space) followed by the expected signature.
Alternatively you can use `/*<-*/` and `/*->*/` as opening and closing parentheses for parts of a signature present in the source but undesired in the documentation (at least at the current stage of development), e.g.

```
def foo/*<-*/()/*->*/: Int
```

will make the expected signature be

```
def foo: Int
```

instead of

```
def foo(): Int
```

Because of the way how signatures in source are parsed, they're expected to span until the end of a line (including comments except those special ones mentioned above, which change the behaviour of tests) so if a definition contains an implementation, it should be placed in a separate line, e.g.

```
def foo: Int
   = 1

class Bar
{
   //...
}
```

Otherwise the implementation would be treated as a part of the signature.

## Roadmap

1. Publish an initial version of the tool together with an SBT plugin
1. Replace Dottydoc as the dedicated tool for documenting Dotty code

   This includes:

   - supporting Dotty's doc pages
   - releasing together with Dotty as the dedicated documentation tool

1. Support all kinds of Dotty definition and generate documentation for the
   standard library
1. Reach feature parity with Scaladoc

## Contributing

We're happy that you'd like to help us!

We have two issue labels you should take a look at: `good first issue` and
`self-contained`. First is easy pickings: you'll be able to contribute without
needing to dive too deep into the project. Second is reverse: it's an issue
that's you may find interesting, complex and self-contained enough that you can
continue chipping away at it without needing to worry too much about merge
conflicts.

To contribute to the project with your code, fork this repo and create a pull request from a fresh branch from there.
To keep the history of commits clean, make sure your commits are squashed into one
and all your changes are applied on top of the latest master branch (if not - rebase on it instead of merging it).
Make sure all the tests pass (simply run `sbt test` to verify that).

## FAQ


### Why use TASTy?

A documentation tool needs to access compiler information about the project - it
needs to list all definitions, resolve them by name, and query their members.
Tasty Reflect is the dedicated way in Scala 3 of accessing this information.

## Credits

- [Flatart](https://www.iconfinder.com/Flatart) - Gitter icon


