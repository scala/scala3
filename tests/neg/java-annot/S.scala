object C {
  val cs: String = "cs"
}

object S {
  @J(C.cs) // error: Annotation argument is not a constant
  def f(): Int = 1
}
