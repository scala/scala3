class C {
  def apply: C
}
object Test {
  (new C)(22)
}
