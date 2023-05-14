package kc

trait Encoder[E] {
  def encode: E

  @SuppressWarnings(Array("foo"))
  def tag1: Encoder[E] = ???

  // Make sure we handle empty Array literals too where we can't guess the Array type from its elements
  @SuppressWarnings(Array())
  def tag2: Encoder[E] = ???
}
