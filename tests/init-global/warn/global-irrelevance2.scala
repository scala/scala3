object A:
  var x = 6

class B(b: Int):
  A.x =  b * 2 // warn

object B:
  new B(10)
