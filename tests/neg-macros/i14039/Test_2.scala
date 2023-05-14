class Dsl {
  val entries = Seq.newBuilder[Any]
  inline def get(): Unit = entries += Macro.apply()
  def test = get() // error
}
