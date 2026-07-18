

object XObject {
  opaque type X = Int

  def anX: X = 5

  given ops: Object with {
    extension (x: X) def + (y: X): X = x + y
  }
}

object MyXObject {
  opaque type MyX = XObject.X

  def anX: MyX = XObject.anX

  given ops: Object with {
    extension (x: MyX) def + (y: MyX): MyX = x + y // warn: warring: Infinite recursive call
  }
}

object Main {
  println(XObject.anX + XObject.anX) // prints 10
  println(MyXObject.anX + MyXObject.anX) // infinite loop
}
