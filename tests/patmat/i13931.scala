class Test:
  def test = Vector() match
    case Seq() => println("empty")
    case _ => println("non-empty")

  def test2 = IndexedSeq() match { case IndexedSeq() => case _ => }
  def test3 = IndexedSeq() match { case IndexedSeq(1) => case _ => }
