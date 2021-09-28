object Test:
  def main(args: Array[String]) =
    try assertTrue(1 == 2) catch e => println(e.getStackTrace()(0))
    try assertTrue(1 == 3) catch e => println(e.getStackTrace()(0))
