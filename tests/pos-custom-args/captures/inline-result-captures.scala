import language.experimental.captureChecking

class Resource extends caps.ExclusiveCapability:
  def use(): Unit = ()
  inline def action(cond: Boolean): Int ?=> Unit = if cond then use()

def condition(): Boolean = true
// Keep the result closure from being beta-reduced before capture checking.
def test(r: Resource^): Unit = r.action(condition())(using 1)
