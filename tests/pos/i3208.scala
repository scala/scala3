class Test {
  trait E
  trait Marker

  def test(es: List[E]): List[E] = es.collect { case e: Marker => e }
}