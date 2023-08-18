trait Selector {
  extension [A](self: A) def at[B <: A]: B
}

trait Config[A, B]

object Field {
  def const[A, B, FieldTpe](selector: Selector ?=> A => FieldTpe): Config[A, B] = ???
}

final case class Example(int: Int)

@main def main = {
  // compiles just fine
  ReproMacro.readPath[Example, Example](Field.const(_.int))

  // doesn't compile
  ReproMacro.readPath[Example, Example](Field.const(_.int.at[Int]))
}
