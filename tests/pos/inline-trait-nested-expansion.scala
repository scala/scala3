inline trait I1:
    def foo = "foo"

inline trait I2:
    def bar = "bar"
 
trait T1 extends I1:
    trait T2 extends I2

def main =
    val a = new T1() {}
    val b = new a.T2() {}
    a.foo
    b.bar
