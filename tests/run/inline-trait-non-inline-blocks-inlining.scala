inline trait Foo:
  def problem = 
    Thread.currentThread.getStackTrace()(1).getClassName()

trait Bar extends Foo

inline trait Confounder

class Baz extends Bar, Confounder

@main def Test = 
  val baz = Baz()
  assert(baz.problem == "Foo")
