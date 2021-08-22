def nonHarmonized[A >: Any](args: A*): String = args.mkString("[", ", ", "]")
def harmonized[A >: Any](args: A*): List[A] = args.toList

@main def Test =
  println(nonHarmonized(3.0, 42))
  println(harmonized(3.0, 42).mkString("[", ", ", "]"))
