import language.experimental.qualifiedTypes.silent

def getInt(): Int = 1

def test: Unit =
  val x: {v: Int with v == 1} = getInt()
