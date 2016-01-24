object IntMap {
  private case object Nil {
    override def equals(that : Any) = that match {
      case _: this.type => true
      case _            => super.equals(that)
    }
  }
}
