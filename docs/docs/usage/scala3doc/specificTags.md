---
title: Dottydoc Specific Tags and Behavior
---

# {{page.title}}

Scala3doc extemds markdown with some unique behaviours such as linking to API. This can be used from within static documentation and blogpost to provide blend-in content.

## Linking to API

If you for instance, want to link to `scala.collection.immutable.Seq` in a
markdown file, you can simply use the canonical path in your url:

```markdown
[Seq](scala.collection.immutable.Seq)
```

Linking to members is done in the same fashion:

```markdown
[Seq](scala.collection.immutable.Seq.isEmpty)
```

Scala3doc denotes objects by ending their names in "$". To select `List.range`
you'd therefore write:

```markdown
[List.range](scala.collection.immutable.List$.range)
```