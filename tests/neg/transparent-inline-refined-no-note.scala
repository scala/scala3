trait Box[A]:
  type Out
  def value: Out

transparent inline def box[A](a: A): Box[A] =
  new Box[A]:
    type Out = A
    def value: A = a

object SavedTest:
  inline def check =
    val b = box("ok")
    val s: String = b.value // error
    // limination: note about transparent inline expansion isn't available
    // if the type mismatch error is reported at the different place than transparent inline call
    // (at `val b = box("ok")`).
