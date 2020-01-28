object XObject {
  opaque type X = Int

  def anX: X = 5

  given ops as Object {
    def (x: X) + (y: X): X = x + y
  }
}

object MyXObject {
  opaque type MyX = XObject.X

  def anX: MyX = XObject.anX

  given ops as Object {
    def (x: MyX) + (y: MyX): MyX = x + y // error: warring: Infinite recursive call
  }
}

object Main extends App {
  println(XObject.anX + XObject.anX) // prints 10
  println(MyXObject.anX + MyXObject.anX) // infinite loop
}
