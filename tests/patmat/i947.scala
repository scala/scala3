object Test {

  class c {

    private var x: Int = 0

    override def equals(other: Any) = other match {
      case o: c => x == o.x
      case xs: List[c] => false
      case ys: List[d18383] => false
      case _ => false
    }


  }
}
