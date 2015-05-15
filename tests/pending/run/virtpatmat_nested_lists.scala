/*
 * filter: It would fail on the following input
 */
object Test extends dotty.runtime.LegacyApp {
  List(List(1), List(2)) match { case x :: (y :: Nil) :: Nil => println(y) }
}
