trait A:
  val x: Int

trait Ctx:
  type T
  val x: T
  val y: T

def f(x: Boolean): A ?=> (c: Ctx) ?=> (Int, c.T) =
  if x then return (summon[A].x, summon[Ctx].x)
  (summon[A].x, summon[Ctx].y)

@main def Test =
  given A:
    val x = 22
  given Ctx:
    type T = String
    val x = "abc"
    val y = "def"

  println(f(true))
  println(f(false))

