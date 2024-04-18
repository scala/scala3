trait GenericCollectionWithCommands {
  self: PackSupport =>

  def bar(foo: Int = 1): Any = ???
  def bar(writer: GenericCollectionWithCommands.this.pack.Writer[Any]): Any = ???
}

trait PackSupport {
  val pack: SerializationPack
}

trait SerializationPack {
  type Writer[A]
}