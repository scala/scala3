object App {
  def coerce[U, V](u: U): V = {
    trait X { type R >: U }
    trait Y { type R = V }

    class T[A <: X](val a: A)(val value: a.R)

    object O { val x : Y & X = x }  // warn

    val a = new T[Y & X](O.x)(u)
    a.value
  }

  def main(args: Array[String]): Unit = {
    val x: Int = coerce[String, Int]("a")
    println(x + 1)
  }
}
