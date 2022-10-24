package covtest

def testForLoop: Unit =
  for i <- 1 to 10 do
    println(i)

def testForAdvanced: Unit =
  def f(x: Int): Boolean = true
  for j <- 1 to 10 if f(j) do
    println(j)

def testForeach: Unit =
  // An anonymous function is created here, but the user code must still be instrumented!
  Nil.foreach(_ => println("user code here"))
