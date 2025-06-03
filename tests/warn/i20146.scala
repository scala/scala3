//> using options -Wunused:imports

def test(list: List[Int]): Int =
  import list.{head => first}
  import list.{length => len} // warn
  import list.{addString => add} // warn
  first + list.length