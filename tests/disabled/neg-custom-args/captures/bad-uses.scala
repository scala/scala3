import caps.use
class Test:
  val bar = (@use c: Test) => () // error
