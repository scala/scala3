//> using options -Ycheck:all
package first

private object Filter :
  private[first] def pass(result: Boolean) = result

class Worker :
  import Filter.pass
  def make(filter: Boolean): String = "done"
  inline def call: String = make(pass(true))

class Test :
  def go = Worker().call
