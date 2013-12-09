object typers {
  
  class List[+T] {
    def :: (x: T) = new :: (x, this)
    
    def len: Int = this match {
      case x :: xs1 => 1 + xs1.len
      case Nil => 0
    }
  }
  
  object Nil extends List[Nothing]
  
  case class :: [+T] (hd: T, tl: List[T]) extends List[T]
  
  def len[U](xs: List[U]): Int = xs match {
    case x :: xs1 => 1 + len(xs1)
    case Nil => 0
  }

}