trait C {}

class O[X]() {
    trait I[Y] {
        def foo(y: Y): Y = y;
    }
    val j:I[X] = null;
}

object p extends O[C]() {
  def c: C = c;
  def main = {
    p.j.foo(c);
  }
}

