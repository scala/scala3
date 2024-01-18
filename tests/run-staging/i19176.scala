import scala.quoted.*

given staging.Compiler =
  object Dummy
  staging.Compiler.make(Dummy.getClass.getClassLoader)

class A
val f: (A, Int) => Int = staging.run { '{ (q: A, x: Int) => x } }

@main def Test = f(new A, 3)
