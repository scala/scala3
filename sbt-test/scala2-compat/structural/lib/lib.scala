class D
object Scala2 {
  val structural1: { type DSub <: D; val member: Int } = new { type DSub <: D; val member: Int = 1 }
  val dsub: structural1.DSub = null.asInstanceOf[structural1.DSub]
  val mbr: structural1.member.type = structural1.member

  def a(a: structural1.DSub): Unit = {}

  def b(a: structural1.member.type): Unit = {}
}
