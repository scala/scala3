object Test {
  type T
  def t1 = Macro.test[Int, String]
  def t2 = Macro.test[Test.type, T]
}
