class Test:
  def callMeth(test: Test) = test.meth(-1)
  private def meth(foo: Int, bar: Int = 1) = foo + bar
