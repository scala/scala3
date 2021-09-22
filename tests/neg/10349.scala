object Firsts:

  type First[X] = X match
    case Map[_, v] => First[Option[v]]

  def first[X](x: X): First[X] = x match
    case x: Map[_, _] => first(x.values.headOption) // error

  @main
  def runFirsts2(): Unit =
    assert(first(Map.empty[Int, Int]) == None) // error
