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

```scala sc:fail sc-opts:-Werror:true
def exampleShouldError(input: Option[String]): Unit = 
  input match
    case Some("foo") => ???   
```

```scala sc:compile sc-opts:-Werror:false
def exampleShouldWarn(input: Option[String]): Unit = 
  input match
    case Some("foo") => ???   
```