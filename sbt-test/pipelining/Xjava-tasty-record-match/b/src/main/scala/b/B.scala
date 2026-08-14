package b

import a.{Rec, RecGen, RecExpl}

// pattern matching on Java records whose symbols are loaded from java TASTy
object B {

  def showRec(r: Rec): String = r match {
    case Rec(i, s) => s * i
  }

  def showRecGen(r: RecGen[String]): String = r match {
    case RecGen(i, s) => s * i
  }

  def showRecExpl(r: RecExpl): String = r match {
    case RecExpl(i, s) => s * i
  }

  @main def test =
    assert(showRec(Rec(2, "ab")) == "abab")
    assert(showRecGen(RecGen(3, "c")) == "ccc")
    assert(showRecExpl(RecExpl(2, "ha")) == "haha")
    assert(showRecExpl(RecExpl(1)) == "default")
}
