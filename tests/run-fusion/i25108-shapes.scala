// The ways a trait val can be overridden in a class that implements the trait's setter.
// None of them may let the setter write the overriding val's field.

trait Logger:
  val prefix: String = "[log]"
  val seenInInit: String = prefix

class Service extends Logger:
  override val prefix: String = "[svc]"

trait Override extends Logger:
  override val prefix: String = "[override]"

class Linearized extends Logger with Override

class Param(override val prefix: String) extends Logger

class Plain extends Logger

@main def Test =
  for l <- List(Service(), Linearized(), Param("[param]"), Plain()) do
    println((l.prefix, l.seenInInit))
