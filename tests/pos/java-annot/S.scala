object C {
  final val cs: "cs" = "cs"
}

object S {
  @J(C.cs) // OK: cs is a constant
  def f(): Int = 1
}
