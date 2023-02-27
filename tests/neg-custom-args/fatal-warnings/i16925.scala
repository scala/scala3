// scalac: -Wunused:all

def hello =
  for {
    i <- 1 to 2 if true
    _ = println(i) // OK
  } yield ()

