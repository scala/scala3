trait Scope
class MScope extends Scope

case class CI(pre: Int, decls: Scope) {
  def derivedCI(pre: Int) = new CI(pre, decls)
  def derivedCI(pre: Int = this.pre, decls: Scope = this.decls) = new CI(pre, decls)
}

object Test {
  def ci = new CI(1, new MScope)
  val decls1 = new MScope
  ci.derivedCI(2, decls = decls1)
  ci.derivedCI(pre = 2)
  ci.derivedCI(decls = decls1)
}
