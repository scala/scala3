class A:
  var a = 20

class B:
  var b = 20

object O:
  val o: A | B = new A
  if o.isInstanceOf[A] then
    o.asInstanceOf[A].a += 1
  else
    o.asInstanceOf[B].b += 1 // o.asInstanceOf[B] is treated as bottom
  o match
    case o: A => o.a += 1
    case o: B => o.b += 1
