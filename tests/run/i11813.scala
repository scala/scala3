trait Ord[A]:
  def name: String
given Ord[Int] with
  def name = "Int"
given Ord[String] with
  def name = "String"

def use(f: [A] => Ord[A] ?=> String): String = f[Int] + "," + f[String]

type F = [A: Ord] => String
def useCB(f: [A: Ord] => String): String = f[Int]

@main def Test =
  println(use([A] => summon[Ord[A]].name))
  println(use([A] => (o: Ord[A]) ?=> o.name))
  val g: [A] => Ord[A] ?=> String = [A] => summon[Ord[A]].name + "!"
  println(g[Int])
  val h: [A] => Ord[A] ?=> Int => String = [A] => (x: Int) => summon[Ord[A]].name * x
  println(h[String](2))

  // context bounds
  val f1 = [A: Ord] => summon[Ord[A]].name
  val f2: [A: Ord] => String = [A: Ord] => summon[Ord[A]].name
  val f3: [A] => Ord[A] ?=> String = [A: Ord] => summon[Ord[A]].name + "?"
  val f4: F = [A] => summon[Ord[A]].name + "!"
  val f5: [A: Ord] => Int => String = [A: Ord] => (n: Int) => summon[Ord[A]].name * n
  println(f1[Int])
  println(f2[String])
  println(f3[Int])
  println(f4[Int])
  println(f5[String](2))
  println(useCB([A: Ord] => summon[Ord[A]].name))
  println(useCB([A] => summon[Ord[A]].name))
