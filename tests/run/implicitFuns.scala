// scalajs: --skip --pending

object Test {
  def main(args: Array[String]): Unit = {

    implicit val world: String = "world!"

    val i1 = ((s: String) ?=> s.length > 2)
    val i2 = {(s: String) ?=> s.length > 2}

    assert(i1)
    assert(i2)

    val x: String ?=> Boolean = { (s: String) ?=> s.length > 2 }

    val xx: (String, Int) ?=> Int = (x: String, y: Int) ?=> x.length + y

    val y: String => Boolean = x(using _)

    object nested {
      implicit val empty: String = ""
      assert(!x)
    }

    val yy: (String, Int) => Any = xx(using _, _)

    val z1: String ?=> Boolean = implicitly[String].length >= 2
    assert(z1)

    type StringlyBool = String ?=> Boolean

    val z2: StringlyBool = implicitly[String].length >= 2
    assert(z2)

    type Stringly[T] = String ?=> T

    val z3: Stringly[Boolean] = implicitly[String].length >= 2
    assert(z3)

    type GenericImplicit[X] = X ?=> Boolean

    val z4: GenericImplicit[String] = implicitly[String].length >= 2
    assert(z4)

    val b = x(using "hello")

    val b1: Boolean = b

    val bi = x

    val bi1: Boolean = bi

    val c = xx(using "hh", 22)

    val c1: Int = c

    Contextual.main(args)

    def foo(s: String): Stringly[Int] = 42

    //(if ("".isEmpty) foo("") else foo("")).apply given ""  // does not typecheck
  }
}

object Contextual {

  class Key[+V]

  class Context(bindings: Map[Key[Any], Any]) {
    def binding[V](key: Key[V]): Option[V] =
      bindings.get(key).asInstanceOf[Option[V]]
    def withBinding[V](key: Key[V], value: V): Context =
      new Context(bindings + ((key, value)))
  }

  val rootContext = new Context(Map())

  val Source = new Key[String]
  val Options = new Key[List[String]]

  type Ctx[T] = Context ?=> T

  def ctx: Ctx[Context] = implicitly[Context]

  def compile(s: String): Ctx[Boolean] =
    runOn(new java.io.File(s))(using ctx.withBinding(Source, s)) >= 0

  def runOn(f: java.io.File): Ctx[Int] = {
    val options = List("-verbose", "-explaintypes")
    process(f).apply(using ctx.withBinding(Options, options))
  }

  def process(f: java.io.File): Ctx[Int] =
    ctx.binding(Source).get.length - ctx.binding(Options).get.length

  def main(args: Array[String]) = {
    implicit val context: Context = rootContext
    assert(compile("abc"))
    assert(compile("ab"))
    assert(!compile("a"))
  }
}

import collection.mutable.ListBuffer

class Transaction {
  private val log = new ListBuffer[String]
  def println(s: String): Unit = log += s

  private var aborted = false
  private var committed = false

  def abort(): Unit = { aborted = true }
  def isAborted = aborted

  def commit(): Unit =
    if (!aborted && !committed) {
      Console.println("******* log ********")
      log.foreach(Console.println)
      committed = true
    }
}

object TransactionalExplicit {

  def transaction[T](op: Transaction => T) = {
    val trans: Transaction = new Transaction
    op(trans)
    trans.commit()
  }

  def f1(x: Int)(implicit thisTransaction: Transaction): Int = {
    thisTransaction.println(s"first step: $x")
    f2(x + 1)
  }
  def f2(x: Int)(implicit thisTransaction: Transaction): Int = {
    thisTransaction.println(s"second step: $x")
    f3(x * x)
  }
  def f3(x: Int)(implicit thisTransaction: Transaction): Int = {
    thisTransaction.println(s"third step: $x")
    if (x % 2 != 0) thisTransaction.abort()
    x
  }

  def main(args: Array[String]) = {
    transaction {
      implicit thisTransaction =>
        val res = f1(args.length)
        println(if (thisTransaction.isAborted) "aborted" else s"result: $res")
    }
  }
}

object Transactional {
  type Transactional[T] = Transaction ?=> T

  def transaction[T](op: Transactional[T]) = {
    implicit val trans: Transaction = new Transaction
    op
    trans.commit()
  }

  def thisTransaction: Transactional[Transaction] = implicitly[Transaction]

  def f1(x: Int): Transactional[Int] = {
    thisTransaction.println(s"first step: $x")
    f2(x + 1)
  }
  def f2(x: Int): Transactional[Int] = {
    thisTransaction.println(s"second step: $x")
    f3(x * x)
  }
  def f3(x: Int): Transactional[Int] = {
    thisTransaction.println(s"third step: $x")
    if (x % 2 != 0) thisTransaction.abort()
    x
  }

  def main(args: Array[String]) = {
    transaction {
      val res = f1(args.length)
      println(if (thisTransaction.isAborted) "aborted" else s"result: $res")
    }
  }
}

object TransactionalExpansion {

  def transaction[T](op: Transaction => T) = {
    val trans: Transaction = new Transaction
    op.apply(trans)
    trans.commit()
  }

  def thisTransaction = ($t: Transaction) => $t

  def f1(x: Int) = { ($t: Transaction) =>
    thisTransaction.apply($t).println(s"first step: $x")
    f2(x + 1).apply($t)
  }
  def f2(x: Int) = { ($t: Transaction) =>
    thisTransaction.apply($t).println(s"second step: $x")
    f3(x * x).apply($t)
  }
  def f3(x: Int) = { ($t: Transaction) =>
    thisTransaction.apply($t).println(s"third step: $x")
    if (x % 2 != 0) thisTransaction.apply($t).abort()
    x
  }

  def main(args: Array[String]) = {
    transaction { $t =>
      val res = f1(args.length).apply($t)
      println(if (thisTransaction.apply($t).isAborted) "aborted" else s"result: $res")
    }
  }
}

object TransactionalAbstracted {
  type Transactional[T] = Transaction ?=> T

  trait TransOps {
    def thisTransaction: Transactional[Transaction]
    def f1(x: Int): Transactional[Int]
    def f2(x: Int): Transactional[Int]
    def f3(x: Int): Transactional[Int]
  }

  object TransOpsObj extends TransOps {

    def thisTransaction: Transactional[Transaction] = implicitly[Transaction]

    def f1(x: Int): Transactional[Int] = {
      thisTransaction.println(s"first step: $x")
      f2(x + 1)
    }
    def f2(x: Int): Transactional[Int] = {
      thisTransaction.println(s"second step: $x")
      f3(x * x)
    }
    def f3(x: Int): Transactional[Int] = {
      thisTransaction.println(s"third step: $x")
      if (x % 2 != 0) thisTransaction.abort()
      x
    }
  }

  val transOps: TransOps = TransOpsObj

  def transaction[T](op: Transactional[T]) = {
    implicit val trans: Transaction = new Transaction
    op
    trans.commit()
  }

  def main(args: Array[String]) = {
    transaction {
      val res = transOps.f1(args.length)
      println(if (transOps.thisTransaction.isAborted) "aborted" else s"result: $res")
    }
  }
}
