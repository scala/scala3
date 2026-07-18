class B

def Test(consume b1: B^, consume b2: B^) =
  def f(): B^ = B()
  var x: B^ = f()
  x = b1 // error separation but should be OK, see #23889
  var y = f()
  y = b2 // error

