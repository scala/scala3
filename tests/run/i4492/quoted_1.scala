
trait Index

object Index {
  transparent def succ(prev: Index): Unit = ~{ '(println("Ok")) }
}
