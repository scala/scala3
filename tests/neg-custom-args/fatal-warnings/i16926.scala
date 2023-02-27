// scalac: -Wunused:all

def hello(): Unit =
  for {
    i <- (0 to 10).toList
    (a, b) = "hello" -> "world" // OK
  } yield println(s"$a $b")
