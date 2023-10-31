trait Foo[TroubleSome] {
  type T <: Foo[TroubleSome]

  this match {
    case e: Foo[?]#T => ???
  }
}
