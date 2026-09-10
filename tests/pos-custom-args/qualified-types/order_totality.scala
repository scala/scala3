def max(x: Int, y: Int): {v: Int with v >= x && v >= y} =
  if x > y then x else y

def min(x: Int, y: Int): {v: Int with v <= x && v <= y} =
  if x < y then x else y

def negatedComparisonVal(a: Int, b: Int): Unit =
  val c = a < b
  if !c then
    val z: Int with z <= a = b
