object Test {
  Nil match {
    case Seq(xs*) => 42
  }
}
