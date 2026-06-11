@main def main =

  // Specification using qualified types
  {
    def zip[A, B](as: List[A], bs: List[B] with bs.size == as.size):
        {l: List[(A, B)] with l.size == as.size}
      = ???

    def concat[T](as: List[T], bs: List[T]):
        {rs: List[T] with rs.size == as.size + bs.size}
      = ???

    val xs: List[Int] = ???
    val ys: List[Int] = ???
    zip(concat(xs, ys), concat(ys, xs))
    zip(concat(xs, ys), concat(xs, xs)) // error
  }

  // Syntax
  {
    {
      type NonEmptyList[A] = { l: List[A] with l.nonEmpty }

      // Not to be confused with structural types
      case class Box(value: Any)
      type IntBox = Box { val value: Int }
    }

    // Shortand
    {
      def zip[A, B](as: List[A], bs: List[B] with bs.size == as.size) = ???
    }
  }

  // Valid/invalid predicates
  {
    {
      var x = 3
      val y: Int with y == 3 = x // error: ⛔️ x is mutable
    }

    {
      val x = 3
      val y: Int with y == 3 = x // okay
    }

    {
      class Box(val value: Int)
      val b: Box with b == Box(3) = Box(3) // error: ⛔️ Box has equality by reference
    }

    {
      case class Box(value: Int)
      val b: Box with b == Box(3) = Box(3) // okay
    }
  }

  // Selfification
  {
    val x: Int = ???
    val y: Int with (y == x + 1) = x + 1

    def f(x: Int): Int = ???
    val z: Int with (z == x + f(x)) = x + f(x)
  }

  // Runtime checks
  {
    val idRegex = "^[a-zA-Z_][a-zA-Z0-9_]*$"
    type ID = {s: String with s.matches(idRegex)}

    {
      "a2e7-e89b" match
        case _: ID => // matched
        case _     => // didn't match
    }

    {
      val id: ID = "a2e7-e89b".runtimeChecked
    }

    {
      val id: ID =
        if ("a2e7-e89b".matches(idRegex)) "a2e7-e89b".asInstanceOf[ID]
        else throw new IllegalArgumentException()
    }

    {
      type Pos = { v: Int with v >= 0 }

      val xs = List(-1,2,-2,1)
      xs.collect { case x: Pos => x } : List[Pos]
    }
  }

  // Subtyping
  {
    {
      val x: Int = ???
      val y: Int = ???
      summon[{v: Int with v == 1 + 1}     =:= {v: Int with v == 2}]
      summon[{v: Int with v == x + 1}     =:= {v: Int with v == 1 + x}]
      summon[{v: Int with v == y + x}     =:= {v: Int with v == x + y}]
      summon[{v: Int with v == x + 3 * y} =:= {v: Int with v == 2 * y + (x + y)}]
    }

    {
      val x: Int = ???
      val y: Int = x + 1
      summon[{v: Int with v == y} =:= {v: Int with v == x + 1}]
    }

    {
      val a: Int = ???
      val b: Int = ???
      summon[{v: Int with v == a && a == b} <:< {v: Int with v == b}]
      def f(x: Int): Int = ???
      summon[{v: Int with a == b}           <:< {v: Int with f(a) == f(b)}]
    }

    {
      summon[3 <:< {v: Int with v == 3}]
    }
  }

  // Backup slides about type-level programming with existing Scala features
  {
    def checkSame(dimA: Int, dimB: dimA.type): Unit = ()
    checkSame(3, 3) // ok
    checkSame(3, 4) // error

    {
      val x = 3
      val y = 3
      checkSame(x, y) // error
    }

    {
      val x2: 3 = 3
      val y3: 3 = 3
      checkSame(x2, y3) // ok
    }

    def readInt(): Int = ???

    {
      val x: Int = readInt()
      val y = x
      val z = y
      checkSame(y, z) // error
    }

    {
      val x: Int = readInt()
      val y: x.type = x
      val z: x.type = x
      checkSame(y, z) // okay
    }


    {
      val x: Int = readInt()
      val y: Int = readInt()
      val z = x + y
      val a = y + x
      checkSame(z, a) // error
    }

    {
      import scala.compiletime.ops.int.+
      val x: 3 = 3
      val y: 5 = 5
      val z: x.type + y.type = x + y
      val a: y.type + x.type = y + x
    }

    {
      import scala.compiletime.ops.int.+
      val x: Int = readInt()
      val y: Int = readInt()
      val z: x.type + y.type = x + y // error
      val a: y.type + x.type = y + x // error
      checkSame(z, a) // error
    }
  }
