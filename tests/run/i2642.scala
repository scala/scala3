// Tests nullary implicit function types
object Test extends App {
  class I
  type X = implicit () => Int
  def ff: X = 2
  assert(ff == 2)
}
