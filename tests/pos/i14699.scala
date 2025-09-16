
package i14699:
  def always(block: => Unit): Unit = ()
  def always(args: Int*)(block: => Unit): Unit = ()

  def test =
    val x = always{}
    val xc: Unit = x
    always(1,2,3) {}

package i15163:
  trait Ctx
  object Ctx:
    given Ctx()
  def always(block: Ctx ?=> Unit): Unit = block
  def always(args: Int*)(block: Ctx ?=> Unit): Unit = block

  def test =
    //val _ = always {}
    val _ = always { (x: Ctx) ?=> () }
    always(1,2,3) {}
