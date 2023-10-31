object Firsts:

  type First[X] = X match
    case Map[?, v] => First[Option[v]]

  def first[X](x: X): First[X] = x match
    case x: Map[?, ?] => first(x.values.headOption) // error

  @main
  def runFirsts2(): Unit =
    assert(first(Map.empty[Int, Int]) == None) // error
