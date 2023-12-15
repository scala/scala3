object A:                             
  val a: Option[Int] = Some(3)
  a match
  case Some(x) => println(x * 2 + B.a.size)
  case None => println(0)

object B:
  val a = 3 :: 4 :: Nil
  a match
  case x :: xs =>
    println(x * 2)
    if A.a.isEmpty then println(xs.size)
  case Nil =>
    println(0)

case class Box[T](value: T)
case class Holder[T](value: T)
object C:
  (Box(5): Box[Int] | Holder[Int])  match
  case Box(x) => x
  case Holder(x) => x

  (Box(5): Box[Int] | Holder[Int])  match
  case box: Box[Int] => box.value
  case holder: Holder[Int] => holder.value

  val a: Int = Inner.b

  object Inner:                
    val b: Int = 10

    val foo: () => Int = () => C.a

    (Box(foo): Box[() => Int] | Holder[Int]) match
    case Box(f) => f()
    case Holder(x) => x

// nopos-error: No warnings can be incurred under -Werror.