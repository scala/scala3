---
layout: doc-page
title: "Binary Compatibility"
---

Thanks to TASTy files, which are produced during compilation of Scala 3 sources, unlike Scala 2, Scala 3 is backward binary compatible between different minor versions (i.e. binary artifacts produced with Scala 3.x can be consumed by Scala 3.y programs as long as x <= y).
There are however already some ongoing attempts to make Scala 3 forward binary compatible which means that, with some restrictions, it might be possible to compile Scala code with a newer version of the compiler and then use the produced binaries as dependencies for a project using an older compiler.

Scala 3.1.2-RC1 adds an experimental `-Yscala-release <release-version>` compiler flag which makes the compiler produce TASTy files that should be possible to use by all Scala 3 compilers in version `<release-version>` or newer (this flag was inspired by how `-release` works for specifying the target version of JDK). More specifically this flag enforces emitting TASTy files in an older format ensuring that:
* the code contains no references to parts of the standard library which were added to the API after `<release-version>` and would crash at runtime when a program is executed with the older version of the standard library on the classpath
* no dependency found on the classpath during compilation (except for the standard library itself) contains TASTy files produced by a compiler newer than `<release-version>` (otherwise they could potentially leak such disallowed references to the standard library)
If any of the checks above is not fulfilled or for any other reason older TASTy cannot be emitted (e.g. the code uses some new language features which cannot be expressed the the older format) the entire compilation fails (with errors reported for each of such issues).

As this feature is experimental it does not have any special support in build tools yet (at least not in sbt 1.6.1 or lower).
E.g. when a project gets compiled with Scala compiler `3.x1.y1` and `-Yscala-release 3.x2` option and then published using sbt
then the standard library in version `3.x1.y1` gets added to the project's dependencies instead of `3.x2.y2`.
When the dependencies are added to the classpath during compilation with Scala `3.x2.y2` the compiler will crash while trying to read TASTy files in the newer format.
A currently known workaround is to modify the build definition of the dependent project by explicitly overriding the version of Scala standard library in dependencies, e.g.

```scala
dependencyOverrides ++= Seq(
  scalaOrganization.value %% "scala3-library" % scalaVersion.value,
  scalaOrganization.value %% "scala3-library_sjs1" % scalaVersion.value // for Scala.js projects
)
```