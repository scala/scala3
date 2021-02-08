
class Test {
  // parses
  def f1 = {
    import scala._;
  }
  // b.scala:7: error: ';' expected but '}' found.
  //   }
  //   ^
  // one error found
  def f2 = {
    import scala.*
  }
  def f2b = {
    import scala.collection.mutable.Map as MMap
  }
  def f(): Unit = {
    locally {
      import scala.util.Properties.lineSeparator
    }
  }

  // parses
  def f3 = {
    import scala.*
    5
  }
  locally { (x: Int) =>
    import scala.util.*
  }
  1 match {
    case 1 => import scala.concurrent.*
  }
}

