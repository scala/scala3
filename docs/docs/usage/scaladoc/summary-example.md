---
title: Summary and example of usage
---

# {{page.title}}

Time to sum-up and check in practice acquired knowledge about scaladoc.

## Basic setup

Ok, we start with as simple `build.sbt` file as it is possible and we are going to gradually extend `docSettings` by providing more and more options.
```scala
lazy val docSettings: Seq[Setting[_]] = Seq()

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-scaladoc-example",
    scalaVersion := scala3Version,
  )
  .settings(docSettings)
```

Currently, our project name is `scala3-scaladoc-example` and this is also the title of generated docs. Let's change this by providing `-project` option and run `sbt doc` to generate documentation.

```scala
lazy val docSettings: Seq[Setting[_]] = Seq(
  Compile / doc / scalacOptions ++= Seq(
    "-project", "scaladoc example",
  )
)
```

## View documentation locally
Generated docs are store at `target/scala-<version>` directory. Let's change target directory to the more toplevel one.

```scala
lazy val docSettings: Seq[Setting[_]] = Seq(
  Compile / doc / target := file("generated-docs"),
  Compile / doc / scalacOptions ++= Seq(
    "-project", "scaladoc example",
  )
)
```

However, in scaladoc `3.2.0` and earlier, so generated documentation is missing a single entry point. This is no longer case in `3.1.x` and hence `"-siteroot", "docs"` is not needed anymore.
```
generated-docs
├── api
├── docs
├── favicon.ico
├── fonts
├── hljs
├── images
├── scaladoc.version
├── scripts
└── styles
```

Let's create directory `docs` when we can put static files. In newly created directory we can create `index.md` file. 
For now, the `index.md` consists of
~~~
Hello, from the scaladoc!

```scala
val n: Int = "5"
```
~~~
You may notice that provided code snippet is invalid and shouldn't compile. Don't worry, we will deal with it later.


Last, but no least, we should add another option to the `docSettings` - `-siteroot` which will point to the directory we created before.

```scala
lazy val docSettings: Seq[Setting[_]] = Seq(
  Compile / doc / target := file("generated-docs"),
  Compile / doc / scalacOptions ++= Seq(
    "-project", "scaladoc example",
    "-siteroot", "docs",
  )
)
```
Now, `generated-docs` directory consists of:

```
generated-docs
├── api
├── favicon.ico
├── fonts
├── hljs
├── images
├── index.html  <<< hurray, there is an index.html which will serve as an entrypoint!
├── scaladoc.version
├── scripts
└── styles
```

To actually view the documentation, the easiest way is to run the following in the project root:

```
cd generated-docs
python3 -m http.server 8080
```
Afterwards, point your browser to <http://localhost:8080>

## Enable type checking snippets
Ok, let's get back to the invalid code snippet we provided earlier. We could add `sc:compile` after language identifier:
~~~
```scala sc:compile
val n: Int = "5"
```
~~~
Thanks to that, scaladoc will catch any compile errors. Running `sbt doc` now yields:
```shell
[error] In static site (.../scaladoc_example/docs/index.md):
[error] At 4:17:
[error]   val n: Int = "5"
[error] Error: Found:    ("5" : String)
[error] Required: Int
[error] (Compile / doc) DottyDoc Compilation Failed
```
However, adding `sc:compile` to every snippet seems a bit cumbersome, there should be a better way. Luckily, there is and `"-snippet-compiler:compile"` comes to the rescue! Thanks to that settings all code snippets will be type checked!

```scala
lazy val docSettings: Seq[Setting[_]] = Seq(
  Compile / doc / target := file("generated-docs"),
  Compile / doc / scalacOptions ++= Seq(
    "-project", "scaladoc example",
    "-siteroot", "docs",
    "-snippet-compiler:compile",
  )
)
```

## Remaining options
Now let's add a few minor options simultaneously. `-project-version`, `-project-logo` and `-project-footer` are pretty self-explanatory and need no one. `-social-links` accepts comma separated list of `<media>::<link>`
```scala
lazy val docSettings: Seq[Setting[_]] = Seq(
  Compile / doc / target := file("generated-docs"),
  Compile / doc / scalacOptions ++= Seq(
    "-project", "scaladoc example",
    "-siteroot", "docs",
    "-snippet-compiler:compile",
    "-project-version", version.value,
    "-project-logo", "docs/logo.svg",
    "-project-footer", s"Footer text",
    "-social-links:github::https://github.com/<account>,twitter::https://twitter.com/<profile>",
)
```

It's time to add last two options, `-source-links` and `revision`, 
Source link provides a mapping between file in documentation and code repository. `revision` specifies a branch or ref, which will be used in mappings in documentation.

```scala
lazy val docSettings: Seq[Setting[_]] = Seq(
  Compile / doc / target := file("generated-docs"),
  Compile / doc / scalacOptions ++= Seq(
    "-project", "scaladoc example",
    "-siteroot", "docs",
    "-snippet-compiler:compile",
    "-project-version", version.value,
    "-project-logo", "docs/logo.svg",
    "-project-footer", s"Footer text",
    "-social-links:github::https://github.com/<repository>,twitter::https://twitter.com/<profile>",
    "-source-links:github://<organization>/<repository>",
    "-revision", "main"
)
```

And that's all! It takes roughly only 15 lines to configure scaladoc, so don't hesitate and give scaladoc a shoot in your library or repository!
