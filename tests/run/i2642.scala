// Tests nullary implicit function types
object Test extends App {
  class I
  type X = given () => Int
  def ff: X = 2
  assert(ff == 2)
}
