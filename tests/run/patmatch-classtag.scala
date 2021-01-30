import reflect.ClassTag
trait API {
  type CaseDef

  implicit val tagForCaseDef: ClassTag[CaseDef]

  trait CaseDefCompanion {
    def apply(x: String): CaseDef
    def unapply(x: CaseDef): Option[String]
  }
  lazy val CaseDef: CaseDefCompanion
}

object dotc {
  case class CaseDef(str: String)
}

object Impl extends API {
  type CaseDef = dotc.CaseDef

  val tagForCaseDef: ClassTag[dotc.CaseDef] = implicitly

  object CaseDef extends CaseDefCompanion {
    def apply(str: String): CaseDef = dotc.CaseDef(str)
    def unapply(x: CaseDef): Option[String] = Some(x.str)
  }
}

object Test extends App {
  val api: API = Impl
  import api.*

  val x: Any = CaseDef("123")

  x match {
    case cdef: CaseDef =>
      val x: CaseDef = cdef
      println(cdef)
  }
  x match {
    case cdef @ CaseDef(s) =>
      val x: CaseDef = cdef
      println(s)
  }
}
