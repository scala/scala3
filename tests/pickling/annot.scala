trait Type
class RefinedType extends Type


object TestAnnot {
  def toText(tp: Type) = tp match {
    case tp: RefinedType =>
      val parent :: (refined: List[RefinedType @unchecked]) = ???
      ???
  }
  val xs: List[RefinedType @unchecked] = ???
}
