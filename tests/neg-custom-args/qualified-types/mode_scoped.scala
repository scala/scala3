def getInt(): Int = 1

def test: Unit =
  val v1: {v: Int with v == 1} = getInt() // error

  locally {
    import language.experimental.qualifiedTypes.silent
    val v2: {v: Int with v == 2} = getInt()
  }

  locally {
    import language.experimental.qualifiedTypes.runtimeChecks
    val v3: {v: Int with v == 3} = getInt()
  }

  val v4: {v: Int with v == 4} = getInt() // error
