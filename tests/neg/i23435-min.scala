
type Or[+A, +B] = A | B

object Test1:
  val x: Or[Int, String] & Or[String, Int] = 3
  val y: Or[Int & String, String & Int] = x // error
  val z: String = y

// shows the distributeAnd logic should not be applied even when
// the targs are pairwise TypeComparer#singletonInterval
object Test2:
  val x: Or["3", Singleton] & Or[Singleton, "3"] = 3
  val y: Or["3", "3"] = x // error
  val z: String = y
