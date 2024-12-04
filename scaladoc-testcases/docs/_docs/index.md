---


---

```scala
val someVariable: Int = 2
```

```scala sc:fail
trait RenderingContext
class Renderer(using RenderingContext)
val renderer: Renderer = Renderer()
```

```scala
  trait Ord:
    type Self

  trait SemiGroup:
    type Self
    extension (x: Self) def combine(y: Self): Self
```

