

class C(val arg: C^)

def main3(x: C^) =
  def c : C^ = new C(x) // error
  c

