---
layout: doc-page
title: "Preview Definitions"
nightlyOf: https://docs.scala-lang.org/scala3/reference/other-new-features/preview-defs.html
---

New Scala language features or standard library APIs are initially introduced as experimental, but once they become fully implemented and acceppted by the [SIP](https://docs.scala-lang.org/sips/) these can become a preview features.
Preview language features and APIs are guaranteed to be standarized in some next Scala minor release, but allow compiler team to introduce small, possibly binary incompatible, changes based on the community feedback.
These can be used by early adopters who can accept possibility of binary compatibility breakage. As an example these can be used for project internal tools and applications, but are discouraged to be used by libraries.

Users can enable access to preview features and definitions by compiling with `-preview` flag. The flag would enable all preview features and definitions. There is no way for enabling only a subset of preview features.

The biggest difference of preview features when compared with experimental features is their non-viral behaviour.
Any defintion compiled in the preview mode (using `-preview` flag) is not marked as preview defintion itself.
This behaviour allows to use preview features transitively in other compilation units without explicitlly enabled preview mode, as long as it does not directly reference APIs or features marked as preview.

The [`@preview`](https://scala-lang.org/api/3.x/scala/annotation/internal/preview.html) annotations are used to mark Scala 3 standard library APIs currently available under enabled preview mode.
The definitions follows similar rules as the [`@experimental`](https://scala-lang.org/api/3.x/scala/annotation/experimental.html) when it comes to accessing, subtyping, overriding or overloading definitions marked with this annotation - all of these can only be performed in compilation unit that enables preview mode.

```scala
//> using options -preview
package scala.stdlib
import scala.annotation.internal.preview

@preview def previewFeature: Unit = ()

// Can be used in non-preview scope
def usePreviewFeature = previewFeature
```

```scala
def usePreviewFeatureTransitively = scala.stdlib.usePreviewFeature
def usePreviewFeatureDirectly = scala.stdlib.previewFeature // error - refering to preview definition outside preview scope
```
