// Exercises the erasure of polymorphic function types.
//
//  - `[X] => (x: A) => B` erases to Function1 (and `[X] => (A, B) => C` to Function2 etc.)
//  - `[X] => T` where `T` is not a function type (e.g. a type parameter) erases to
//    ErasedPolyFunction, which is a Function0 at runtime.
//
// Values flow between the two representations whenever a type parameter standing for
// the result of a polymorphic function gets instantiated to a function type. Erasure
// then has to adapt in both directions, and when boxing to Object/Any/type parameters.

object Test:

  // Poly functions with an abstract result type, erasing to ErasedPolyFunction.
  def bar[A](ff: [X] => A): A = ff[Unit]
  def idPoly[A](ff: [X] => A): [X] => A = ff
  def bar2[A](ff: [X, Y] => A): A = ff[Int, String]
  def idPoly2[A](ff: [X, Y] => A): [X, Y] => A = ff

  // Poly functions with a function result type, erasing to FunctionN.
  val f0 = [X] => () => { println("f0"); 42 }
  val f1 = [X] => (y: Object) => { println(y); y }
  val f2 = [X] => (a: Int, b: Int) => a + b
  val f3 = [X, Y] => (a: Int, b: Boolean, c: String) => s"$a-$b-$c"

  var counter = 0
  def effectful(): [X] => (y: Object) => Object =
    counter += 1
    f1

  trait Runner[A]:
    def run(ff: [X] => A): A
    def runTwice(ff: [X] => A): A = { run(ff); run(ff) }

  class Fun1Runner extends Runner[Object => Object]:
    def run(ff: [X] => Object => Object): Object => Object =
      val g = ff[String]
      g("in Fun1Runner")
      g

  class Fun2Runner extends Runner[(Int, Int) => Int]:
    def run(ff: [X] => (Int, Int) => Int): (Int, Int) => Int =
      println(ff[Int](1, 2))
      ff[Int]

  class Holder[A](val ff: [X] => A):
    def get: A = ff[Int]
    def getAs[B]: B = ff[B].asInstanceOf[B]

  def main(args: Array[String]): Unit =

    println("--- FunctionN -> ErasedPolyFunction -> FunctionN, various arities")
    val a0 = bar(f0)
    println(a0())
    val a1 = bar(f1)
    println(a1("a1"))
    val a2 = bar(f2)
    println(a2(1, 2))
    val a3 = bar2(f3)
    println(a3(1, true, "c"))
    val a1b = bar([X] => (y: Object) => { println(s"literal $y"); y })
    a1b("a1b")

    println("--- explicit type arguments")
    val a1c = bar[Object => Object](f1)
    a1c("a1c")

    println("--- ErasedPolyFunction result re-adapted to FunctionN")
    val h0: [X] => () => Int = idPoly(f0)
    println(h0[String]())
    println(h0())
    val h1: [X] => Object => Object = idPoly(f1)
    h1[Int]("h1")
    h1("h1 inferred")
    val h2 = idPoly(f2)
    println(h2[Unit](3, 4))
    println(h2(5, 6))
    val h3 = idPoly2(f3)
    println(h3[Int, Boolean](7, false, "h3"))
    println(h3(8, true, "h3 inferred"))
    println(idPoly(idPoly(idPoly(f2)))(10, 20))

    println("--- partial type application yields an ordinary function")
    val g1: Object => Object = f1[Int]
    g1("g1")
    val g2: (Int, Int) => Int = f2[String]
    println(g2(1, 1))
    val g3 = f3[Int, Int]
    println(g3(1, true, "g3"))
    println(idPoly2(f3)[String, String](2, false, "g3 via idPoly2"))

    println("--- partial term application and eta-expansion")
    def applyFirst(ff: [X] => (Int, Int) => Int, a: Int): Int => Int = ff(a, _)
    println(applyFirst(f2, 100)(1))
    println(applyFirst(idPoly(f2), 200)(2))
    val curried: Int => Int => Int = f2[Unit].curried
    println(curried(1000)(1))
    val tupled: ((Int, Int)) => Int = f2[Unit].tupled
    println(tupled((2000, 1)))
    def toPoly(g: Object => Object): [X] => Object => Object = [X] => (y: Object) => g(y)
    bar(toPoly(f1[String]))("eta")

    println("--- non-idempotent arguments are evaluated exactly once")
    counter = 0
    val e1 = bar(effectful())
    println(s"counter = $counter")
    e1("e1")
    println(s"counter = $counter")
    val e2: [X] => Object => Object = idPoly(effectful())
    println(s"counter = $counter")
    e2("e2")
    e2("e2 again")
    println(s"counter = $counter")

    println("--- boxing to Object, Any and type parameters")
    val anyF: Any = f1
    val objF: Object = f2
    val polyFromAny = anyF.asInstanceOf[[X] => Object => Object]
    polyFromAny("from Any")
    println(objF.asInstanceOf[[X] => (Int, Int) => Int](3, 3))
    println(anyF.isInstanceOf[Function1[?, ?]])
    println(objF.isInstanceOf[Function2[?, ?, ?]])
    def identity[T](t: T): T = t
    identity(f1)("identity")
    val list = List(f1, [X] => (y: Object) => { println(s"second $y"); y })
    for g <- list do g[Int]("list")
    val fromList: [X] => Object => Object = list.head
    fromList("fromList")
    val opt: Option[[X] => Object => Object] = Some(f1)
    opt.get("opt")
    opt.map(g => g("opt.map"))
    val polyAny: [X] => Any = f1
    println(polyAny[Int].isInstanceOf[Function1[?, ?]])
    polyAny[Int].asInstanceOf[Object => Object]("polyAny")
    val polyObj: [X] => Object = idPoly(f2)
    println(polyObj[Int].asInstanceOf[(Int, Int) => Int](4, 4))

    println("--- overriding and bridges")
    val r1: Runner[Object => Object] = Fun1Runner()
    r1.run(f1)("r1")
    r1.runTwice(f1)("r1 twice")
    val r2: Runner[(Int, Int) => Int] = Fun2Runner()
    println(r2.run(f2)(6, 6))
    println(r2.runTwice(idPoly(f2))(7, 7))
    println(Fun2Runner().run(f2)(8, 8))

    println("--- class with poly function field")
    val hold = Holder(f1)
    hold.get("hold.get")
    hold.ff("hold.ff")
    hold.ff[Int]("hold.ff[Int]")
    println(hold.getAs[Object => Object]("getAs"))
    val holdVar = new Holder[Object => Object](idPoly(idPoly(f1)))
    holdVar.ff("holdVar")

    println("--- vars, lazy vals, by-name, defaults")
    var v: [X] => Object => Object = f1
    v("v")
    v = idPoly(f1)
    v("v reassigned")
    lazy val lz: [X] => (Int, Int) => Int = idPoly(f2)
    println(lz(9, 9))
    def byName(ff: => [X] => Object => Object): Unit = { ff("byName"); ff[Int]("byName[Int]") }
    byName(f1)
    byName(idPoly(f1))
    def withDefault(ff: [X] => (Int, Int) => Int = f2): Int = ff(10, 10)
    println(withDefault())
    println(withDefault(idPoly(f2)))

    println("--- poly functions returned from and matched in expressions")
    val picked: [X] => Object => Object = if counter > 0 then f1 else idPoly(f1)
    picked("picked")
    def choose(n: Int): [X] => (Int, Int) => Int = n match
      case 0 => f2
      case _ => idPoly(f2)
    println(choose(0)(1, 2))
    println(choose(1)(3, 4))
    bar(f1) match
      case g: (Object => Object) => g("matched")
