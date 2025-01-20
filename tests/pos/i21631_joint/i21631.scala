class Channel extends AbstractChannel() {
  override def newUnsafe(): AbstractChannel#AbstractUnsafe = new AbstractUnsafe {
    override def connect(): Unit = ???
  }
}
