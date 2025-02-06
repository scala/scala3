class Test:
  def test = Vector() match
    case Seq() => println("empty")
    case null => println("non-empty")

  def test2 = IndexedSeq() match { case IndexedSeq() => case null => }
  def test3 = IndexedSeq() match { case IndexedSeq(1) => case _ => }
