import scala.quoted.*

given staging.Compiler =
  staging.Compiler.make(getClass.getClassLoader.getParent) // we want to make sure the classloader is incorrect

class A

@main def Test =
  try
    val f: (A, Int) => Int = staging.run { '{ (q: A, x: Int) => x } }
    f(new A, 3)
  catch case ex: Exception =>
    assert(ex.getMessage().startsWith("An unhandled exception was thrown in the staging compiler."), ex.getMessage())
    println("exception thrown, no additional printlns")
