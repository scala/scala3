trait Bad extends Any {
  override def equals(other: Any): Boolean = ???
  override def hashCode: Int = ???
}

trait OK extends Any {
  def equals(other: OK) = ???  // OK since no override
  def hashCode: Int            // OK since abstract
}
