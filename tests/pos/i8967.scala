inline def aToB[T,A,B](t:T,b:B): T|B = t match {
  case _:A => b
  case _:T => t
}

@main def main() = aToB[Int, Double, String](1,"x")
