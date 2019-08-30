trait F[T] { type X }
object Rec extends F[Rec.X] // error: X is not a member of Rec
