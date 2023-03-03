object A {
  case class AnotherCaseClass(name: String)

  val errorsOut1 = ReproTransformer.getTransformer[A.AnotherCaseClass, AnotherCaseClass]
  val errorsOu2 = ReproTransformer.getTransformer[AnotherCaseClass, A.AnotherCaseClass]

  val works1 = ReproTransformer.getTransformer[A.AnotherCaseClass, A.AnotherCaseClass]
  val works2 = ReproTransformer.getTransformer[AnotherCaseClass, AnotherCaseClass]
}
