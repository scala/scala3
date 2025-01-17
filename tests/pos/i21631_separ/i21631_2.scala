class Channel extends AbstractChannel_1() {
  override def newUnsafe(): AbstractChannel_1#AbstractUnsafe = new AbstractUnsafe {
    override def connect(): Unit = ???
  }
}
