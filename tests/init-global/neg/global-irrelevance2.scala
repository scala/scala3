object A:
  var x = 6

class B(b: Int):
  A.x =  b * 2 

object B:
  new B(10)

// nopos-error: No warnings can be incurred under -Werror.