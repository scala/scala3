# Scala3doc

Scala3doc (name subject to change) is the documentation tool for
[Dotty](https://github.com/lampepfl/dotty), which is scheduled to become
Scala 3. It's based on [Dokka](https://github.com/Kotlin/dokka), the
documentation tool for Kotlin. It uses Tasty-Reflect to access definitions,
which is an officially supported way to access Dotty's perspective of a
codebase.

We're aiming to support all the features Scaladoc did, plus new and exciting ones such as:

- Markdown syntax!
- displaying project and API documentation together on one site!
- and more!

## Contributing

We're happy that you'd like to help us!

We have two issue labels you should take a look at: `good first issue` and
`self-contained`. First is easy pickings: you'll be able to contribute without
needing to dive too deep into the project. Second is reverse: it's an issue
that's you may find interesting, complex and self-contained enough that you can
continue chipping away at it without needing to worry too much about merge
conflicts.

## Running the project

Use the following commands to generate documentation for this project and for Dotty, respectively:

```
sbt generateSelfDocumentation
sbt generateDottyLibDocumentation
```

To actually view the documentation, the easiest way is to run the following in project root:

```
cd output
python3 -m http.server 8080
```

And afterwards point your browser to `http://localhost:8080/self` or
`http://localhost:8080/stdLib` for this project and for Dotty documentation
respectively.

It's not strictly necessary to go through an HTTP server, but because of CORS
the documentation won't work completely if you don't.

## Developing

At least two of our contributors use [Metals](https://scalameta.org/metals/) to
work on the project.

For every PR, we build documentation for Scala3doc and Dotty. For example, for
PR 110 you can find them at:

+ https://contextbuddy.s3.eu-central-1.amazonaws.com/dokka-dotty/pr-110/self/main/index.html
+ https://contextbuddy.s3.eu-central-1.amazonaws.com/dokka-dotty/pr-110/stdLib/main/index.html

Note that these correspond to the contents of `output` directory - that's
precisely what they are.

You can also find the result of building the same sites for latest `master` at:

+ https://contextbuddy.s3.eu-central-1.amazonaws.com/dokka-dotty/pr-master/self/main/index.html
+ https://contextbuddy.s3.eu-central-1.amazonaws.com/dokka-dotty/pr-master/stdLib/main/index.html

## Roadmap

1. Publish an initial version of the tool together with an SBT plugin
1. Replace Dottydoc as the dedicated tool for documenting Dotty code

   This includes:
   + supporting Dotty's doc pages
   + releasing together with Dotty as the dedicated documentation tool

1. Support all kinds of Dotty definition and generate documentation for the
   standard library
1. Reach feature parity with Scaladoc

## FAQ

### Why depend on Dokka?

We have two primary reasons for depending on Dokka. One of them is division of
labour - Dokka already has a team of maintainers, and it supports an excellent
API which already allowed us to quite easily generate documentation with it. By
depending on Dokka, we will be able to share a large portion of the maintenance
burden. The second reason is very pragmatic - on our own, it'd be difficult for
us to reach even feature parity with Scaladoc, simply because of workforce
constraints. Meanwhile, Dokka maintainers from VirtusLab reached out to us with
an offer of help, which we were happy to take.

### Why use TASTy?

A documentation tool needs to access compiler information about the project - it
needs to list all definitions, resolve them by name, and query their members.
Tasty Reflect is the dedicated way in Scala 3 of accessing this information.
