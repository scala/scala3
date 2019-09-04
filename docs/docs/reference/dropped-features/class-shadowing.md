---
layout: doc-page
title: Dropped: Class Shadowing
---

Scala so far allowed patterns like this:

    class Base {
      class Ops { ... }
    }

    class Sub extends Base {
      class Ops { ... }
    }

Dotty rejects this with the error message:

    6 |      class Ops {  }
      |            ^
      |class Ops cannot have the same name as class Ops in class Base -- class definitions cannot be overridden

The issue is that the two `Ops` classes _look_ like one overrides the
other, but classes in Scala cannot be overridden. To keep things clean
(and its internal operations consistent) the Dotty compiler forces you
to rename the inner classes so that their names are different.

[More details](./class-shadowing-spec.md)
