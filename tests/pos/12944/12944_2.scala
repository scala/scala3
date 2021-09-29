import Test1._

object Test2 {
  val fails = summon[Bar[2] =:= "2"]
}
