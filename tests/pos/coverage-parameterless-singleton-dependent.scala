class Source:
  val x = new Object
  def get: x.type = x

def acceptSingleton(x: AnyRef)(y: x.type): Unit = ()

def testSingleton(source: Source): Unit =
  acceptSingleton(source.get)(source.get)
