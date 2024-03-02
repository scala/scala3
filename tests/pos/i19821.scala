type F[X] = X match { case String    => Option[Int] }
type G[X] = X match { case Option[x] => Int         }

trait T:
  type S
  val opt1: F[S]
  val opt2: O1
  type O1 = F[S]

class Test:
  def test: Unit =
    val t: T { type S = String } = ???

    val i1: G[t.opt1.type] = ???
    val j1: Int = i1

    val i2: G[t.opt2.type] = ???
    val j2: Int = i2 // was:
//[E007] Type Mismatch Error: tests/pos/i19821.scala:17:18 --------------------
//   val j2: Int = i2
//                 ^^
//                 Found:    (i2 : G[(t.bar : t.O)])
//                 Required: Int
