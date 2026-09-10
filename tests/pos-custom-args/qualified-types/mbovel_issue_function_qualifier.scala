import language.experimental.qualifiedTypes.silent

case class Structure[S, T](
  valid: S => Boolean,
  single: T => {s: S with valid(s)})

object IntListStructure:
  type S = List[Int]
  type T = Int
  val valid = (s: S) => s.nonEmpty
  val single: T => S = (t: T) => List(t)
  def get = Structure[S, T](valid, single)
