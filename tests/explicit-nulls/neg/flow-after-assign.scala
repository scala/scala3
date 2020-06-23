class C(val x: Int, val next: C|Null)

def f = {
  var xs: C|Null = C(1, C(2, null))
  while (xs != null) {
    val xsx: Int = xs.x
    val xscpy: C = xs
    xs = xscpy // Since xscpy is non-nullable, after the assign, xs is still non-nullable
    val xscpyx: Int = xscpy.x
    xs = xs.next // xs.next is nullable, after the assign, xs becomes nullable
    val xsnx: Int = xs.x // error
  }
}