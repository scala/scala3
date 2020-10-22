---
layout: doc-page
title: Working with Scalafix
---

# Working with Scalafix

First, create a new rule as follows (command from https://scalacenter.github.io/scalafix/docs/developers/setup.html):

```bash
sbt new scalacenter/scalafix.g8 --repo="Repository Name"
```

To run the rule against some codebase:

```bash
scalafix -r file:scalafix/rules/src/main/scala/fix/YourRule.scala your/code/base/
```

Where `YourRule.scala` is the rule you developed and `your/code/base` is the code base you are running the rule against.
