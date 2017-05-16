object Types {
  abstract case class TermRef(val prefix: String, name: String) {
    type ThisType = TermRef

    def alts: List[TermRef] = ???
  }
}

class Test {
  def foo(tp: Types.TermRef): Unit = {
      tp.alts.filter(_.name == "apply") match {
        case Nil =>
        case alt :: Nil =>
        case alt =>
      }
  }
}