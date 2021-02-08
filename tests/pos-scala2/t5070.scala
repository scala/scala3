trait Web {
  type LocalName
}
trait Companion1[A]
trait WebDSL[W <: Web] {
  trait LocalNameCompanion extends Companion1[W#LocalName] {
    type A = String
  }
  implicit val LocalName: LocalNameCompanion
}
object Test {
  def t[W <: Web](implicit webDSL: WebDSL[W]): Unit = {
    import webDSL.*
    implicitly[LocalNameCompanion] // succeeds
    implicitly[Companion1[W#LocalName]] // fails
  }
}

