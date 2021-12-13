class Test:
  def test = Vector() match
    case Seq() => println("empty")
    case _ => println("non-empty")
