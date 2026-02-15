package covtest

/**
  * Type Lambdas: https://xxxxx.xxxx.xx/docs/reference/new-types/type-lambdas.html
  * Taken from https://github.com/scala/scala3-example-project
  */
object TypeLambdas:

  type M = [X, Y] =>> Map[Y, X]

  type Tuple = [X] =>> (X, X)

  def test(): Unit =
    val m: M[String, Int] = Map(1 -> "1")
    println(m)

    val tuple: Tuple[String] = ("a", "b")
    println(tuple)

