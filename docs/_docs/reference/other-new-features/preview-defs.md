---
layout: doc-page
title: "Preview Definitions"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/preview-defs.html
---

The [`@preview`](https://scala-lang.org/api/3.x/scala/annotation/preview.html) annotation allows the definition of an API that is not guaranteed backward binary, but might become stable in next minor version of the compiler.

New Scala language features or standard library APIs initially introduced as experimental can become a preview features when they have become fully implemented and acceppted by the [SIP](https://docs.scala-lang.org/sips/) before they're accepted as standard features.
Such definitions can be used by early adopters that can accept possibility of binary compatibility breakage, for example these can be used for project internal tools and applications, but are discouraged to be used by libraries.

The [`@preview`](https://scala-lang.org/api/3.x/scala/annotation/preview.html) definitions follows similar rules as the [`@experimental`](https://scala-lang.org/api/3.x/scala/annotation/experimental.html) - to enable access to preview feature or API in given compilation unit Scala compiler requires either:

- explicit `-preview` flag passed to the compiler,
- top level import for explicit `scala.language.preview.<feature>`,
- annotating defintion that referes to preview feature with `@preview`

The biggest difference of preview features when compared with experimental features is their non-viral behaviour.
Any defintion that was compiles in the preview scope (using `-preview` flag or `scala.language.preview` top-level import) is not annotated as `@preview` defintion itself. It behaviour allows to use preview features transitively in other compilation units without enabled preview mode.

```scala
//> using options -preview
import scala.annotation.preview

@preview def previewFeature: Unit = ()

// Can be used in non-preview scope
def usePreviewFeature = previewFeature
```

```scala
def usePreviewFeatureTransitively = usePreviewFeature
def usePreviewFeatureDirectly = previewFeature // error - refering to preview definition outside preview scope
def useWrappedPreviewFeature = wrappedPreviewFeature // error - refering to preview definition outside preview scope

@scala.annotation.preview
def wrappedPreviewFeature = previewFeature
```
