//> using options -source 3.0-migration 

def f: List[Int] = {
  List(1, 2, 3).map  // no newline inserted here in Scala-2 compat mode
  { x =>    // warn (migration)
    x + 1
  }
}

