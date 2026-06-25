trait Typeclass[I]

sealed trait Enumm:
  type Insider

object Enumm:
  case object Enumm1 extends Enumm:
    case class Insider()

    object Insider:
      given t: Typeclass[Insider] = new Typeclass[Insider] {}

class Foo
implicit val f: Foo = new Foo

def pathDependentTypeDefaultParam(tpe: Enumm)(i: tpe.Insider, p: Int = 0)(using t: Typeclass[tpe.Insider]): Int = p
def defaultParamNotPDT(tpe: Enumm)(i: tpe.Insider, p: Int = 0)(using t: Foo): Int = p
def pathDependentTypeDefaultParamOwnList(tpe: Enumm)(i: tpe.Insider)(p: Int = 0)(using t: Typeclass[tpe.Insider]): Int = p

def main =
  pathDependentTypeDefaultParam(Enumm.Enumm1)(Enumm.Enumm1.Insider())
  pathDependentTypeDefaultParam(Enumm.Enumm1)(Enumm.Enumm1.Insider(), p = 0)
  defaultParamNotPDT(Enumm.Enumm1)(Enumm.Enumm1.Insider())
  pathDependentTypeDefaultParamOwnList(Enumm.Enumm1)(Enumm.Enumm1.Insider())()
  pathDependentTypeDefaultParamOwnList(Enumm.Enumm1)(Enumm.Enumm1.Insider())(p = 0)
