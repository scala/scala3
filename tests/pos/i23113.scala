//> using options -Werror
sealed trait P
object P {
  private abstract class B extends P
}

def foo(opt: Option[P]): Unit = opt match {
  case Some(_) => ???
  case None    => ???
}
