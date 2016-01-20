object Test {
  def second[sA, sB <: sA](foo: sA, bar: sB): sB = bar
  def third[A, B >: A](foo: A, bar: B): B = bar
  def expectString(s: String) = s

  def test = {
    val x = second(Set.empty[String], Set.empty)
    x map expectString
    second(Set.empty[String], Set.empty) map expectString
    third(Set.empty[String], Set.empty) map expectString
  }
}
