class Contra[-D](task: AnyRef|Null)
object Test {
  def narrow(task: AnyRef|Null): Contra[task.type] = new Contra(task)
  def ident[Before](elems: Contra[Before]): Contra[Before] = elems
  val foo = null
  ident(narrow(foo))
}
