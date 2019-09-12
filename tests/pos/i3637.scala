import reflect.ClassTag

object Demo {
  def summon[T](implicit ev: T): ev.type = ev // More precise implicitly, needed to crash

  {
    case class B(i: Int)
    summon[ClassTag[B]] // Has to be the last statement of the block
  }
}
