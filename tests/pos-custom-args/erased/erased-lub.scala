// Verify that expressions below perform correct boxings in erasure.
object Test {
  def id[T](t: T) = t

  val x = true
  val one = 1

  { if (x) id(one) else 0 } + 1

  { if (x) new scala.util.Random()}.asInstanceOf[Runnable]

  { x match {
      case true => id(one)
      case _ => 0
    }
  } + 1

  { try {
      id(one)
    } catch {
      case ex: Exception => 0
    }
  }.asInstanceOf[Runnable]

  val arr = Array(id(one), 0)

}
