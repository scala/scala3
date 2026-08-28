object Use {
  def Test = scala.compiletime.testing.typeCheckErrors("stackOverflowMacro") // error
}
