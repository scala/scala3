class Contra[-D](task: AnyRef)
object Test {
  def narrow(task: AnyRef): Contra[task.type] = new Contra(task)
  def ident[Before](elems: Contra[Before]): Contra[Before] = elems
  val foo = null
  ident(narrow(foo))
}
