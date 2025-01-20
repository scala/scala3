class A(o: O):
  var a = 20

class B(o: O):
  var b = 20

class O:
  val o: A | B = new A(this)
  if o.isInstanceOf[A] then
    o.asInstanceOf[A].a += 1
  else
    o.asInstanceOf[B].b += 1 // o.asInstanceOf[B] is treated as bottom

  // prevent early promotion
  val x = 10
