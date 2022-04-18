trait Fruit
case class Apple() extends Fruit
case class Orange() extends Fruit

case class Box[C](fruit: C) extends Fruit

val apple = Box(fruit = Apple())
val orange = Box(fruit = Orange())


val result = List(apple, orange).map {
  case appleBox: Box[Apple] @unchecked if appleBox.fruit.isInstanceOf[Apple] => //contains apple
    "apple"
  case _ =>
    "orange"
}

@main def Test =
  assert(result == List("apple", "orange"))
