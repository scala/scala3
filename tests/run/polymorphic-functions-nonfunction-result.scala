// Polymorphic function literals whose result type is not a function type
trait Ord[A] { def name: String }
given Ord[Int] with { def name = "Int" }

def use(f: [A] => AnyRef): AnyRef = f[Int]

type Res = Any

@main def Test =
  val log = collection.mutable.ListBuffer[String]()

  val f1: [A] => Any = [A] => (x: A) => { log += "body"; x }
  val g1 = f1[Int]
  log += "applied"
  assert(g1.asInstanceOf[Int => Int](1) == 1)
  assert(log.toList == List("applied", "body"))

  val f2: [A] => Res = [A] => (o: Ord[A]) ?=> o.name
  assert(f2[Int].asInstanceOf[Ord[Int] => String](summon[Ord[Int]]) == "Int")

  assert(use([A] => (x: A) => x) != null)

  val f3: [A] => (A => A) | Null = [A] => (x: A) => x
  assert(f3[Int].nn(3) == 3)

  val f4: [A] => Matchable = [A] =>
    (x1: A, x2: A, x3: A, x4: A, x5: A, x6: A, x7: A, x8: A, x9: A, x10: A, x11: A, x12: A,
     x13: A, x14: A, x15: A, x16: A, x17: A, x18: A, x19: A, x20: A, x21: A, x22: A, x23: A) => x23
  assert(f4[Int].isInstanceOf[scala.runtime.FunctionXXL])
