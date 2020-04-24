def f: List[Int] = {
  List(1, 2, 3).map  // no newline inserted here in Scala-2 compat mode
  { x =>    // error (migration)
    x + 1
  }
}
