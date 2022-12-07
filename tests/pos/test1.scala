def max(xs: List[Int]): Int =
  println(xs)
  if xs.isEmpty then throw new java.util.NoSuchElementException("xs is empty")
  else if xs.tail.isEmpty then xs.head
  else xs.head max max(xs.tail)

@main def run = max(List(1, 2, 3))

