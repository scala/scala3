object Foo {
  override def equals(that : Any) = that match {
    case _: this.type => true
    case _            => false
  }
}

@main def Test =
  println(Foo.equals(Foo))
  println(Foo.equals(new AnyRef {}))
  println(Foo.equals(0))
