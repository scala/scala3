---
layout: index
title: Error codes
---

The Scala 3 compiler assigns a unique error code to each type of error it can emit.
These codes appear in compiler output in the format `[E001]`, `[E007]`, etc.

This section documents each error code with:

- **Description** — what the error means and when it occurs
- **Example** — code that triggers the error
- **Error output** — the actual compiler message you'll see
- **Solution** — how to fix the problem

## Using error codes

When you encounter an error, you can use the `-explain` flag to get more detailed information:

```bash
scala compile -explain MyFile.scala
```

You can also look up any error code directly in this documentation by its number (e.g., E007 for type mismatch errors).

Browse the full list in the sidebar to find documentation for any specific error code.

