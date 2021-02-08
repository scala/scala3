import blah.*

def testO(): Unit = {
  import AsObject.LineNo
  assert(summon[LineNo].lineNo == 4)
}

def testP(): Unit = {
  import AsPackage.LineNo
  assert(summon[LineNo].lineNo == 9)
}

@main def Test =
  testO()
  testP()
