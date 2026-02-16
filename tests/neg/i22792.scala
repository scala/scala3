//> using options -explain

trait Permit
class Foo:
  def run(implicit ev: Permit): Unit = ???

given Permit = ???
@main def Test = new Foo().run() // error
