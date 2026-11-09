def select[A](s: Seq[() => A]) =
  val x = s(0) // sep error
  val _: () -> A = x // error
  val y = s.head // sep error
  val _: () -> A = y // error

def select2[A, C^](s: Seq[() ->{C} A]) =
  val x = s(0)
  val _: () -> A = x // error
  val y = s.head
  val _: () -> A = y // error





