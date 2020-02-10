trait Evidence[X]

trait Trait[X : Evidence]:
  def method(x : X) : X

given ev as Evidence[Int] = new Evidence[Int]{}
val crash : Trait[Int] = (x: Int) => x          // error
