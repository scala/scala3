trait Evidence[X]

trait Trait[X : Evidence] with
  def method(x : X) : X

given ev: Evidence[Int] = new Evidence[Int]{}
val crash : Trait[Int] = (x: Int) => x          // error
