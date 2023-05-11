class Denotation
abstract class SingleDenotation extends Denotation
def goRefined: Denotation =
  val foo: Denotation = ???
  val joint = foo
  joint match
    case joint: SingleDenotation =>
      joint
    case _ =>
      joint