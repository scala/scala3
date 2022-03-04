package example 

/**
  * Trait Parameters: https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html
  * Taken from https://github.com/scala/scala3-example-project
  */
object TraitParams:

  trait Base(val msg: String)
  class A extends Base("Hello")
  class B extends Base("Dotty!")

  // Union types only exist in Scala 3, so there's no chance that this will accidentally be compiled with Scala 2
  private def printMessages(msgs: (A | B)*) = println(msgs.map(_.msg).mkString(" "))

  def test(): Unit =
    printMessages(new A, new B)

    // Sanity check the classpath: this won't run if the Scala 3 jar is not present.
    val x: Int => Int = identity
    x(1)

