object Test {

  abstract class C { type T; val x: T }

  val c = new C { type T = Int; val x = 1 }

  object X { def unapply(x: C): Some[x.T] = Some(x.x) }

  val y = c match { case X(y) => y }
  val y1: Int = y

  val z = (c: Any)  match { case X(y) => y }
  // val z1: C#T = z  // error: z has type Any TODO: find out why
}
