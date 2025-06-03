---
name: "\U0001F615 Error/Warning message report"
about: Report an error/warning message that was confusing/unhelpful
title: ''
labels: itype:enhancement, area:reporting, better-errors, stat:needs triage
assignees: ''

---

## Compiler version

If you're not sure what version you're using, run `print scalaVersion` from sbt
(if you're running scalac manually, use `scalac -version` instead).

## Minimized example

<!--
This code should be self-contained, reproducible (i.e. produces the expected error/warning message) and as small as possible.

Ideally, we should be able to just copy this code in a file and run `scalac` (and maybe `scala`) to reproduce the issue.

For a good example, see https://github.com/scala/scala3/issues/18657
-->

```Scala
printl("hello, world")
```

## Output Error/Warning message

<!--
Here, please provide the produced error/warning message that is confusing/unhelpful, etc.

for example:
-->

```scala
-- [E006] Not Found Error: ----------------
1 |printl("hello, world")
  |^^^^^^
  |Not found: printl
1 error found
```

## Why this Error/Warning was not helpful

<!-- For a good example, see https://github.com/scala/scala3/issues/18657 -->

The message was unhelpful because...

## Suggested improvement

<!-- For a good example, see https://github.com/scala/scala3/issues/18657 -->

It could be made more helpful by...
