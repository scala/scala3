
trait Index

object Index {
  rewrite def succ(prev: Index): Unit = ~{ '(println("Ok")) }
}
