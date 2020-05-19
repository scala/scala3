
def rainWater(vec: List[Int]): Int =
  if vec.isEmpty then 0
  else
    val mx = vec.max
    val split = vec.indexWhere(_ == mx)
      rainWater1(0, vec.take(split + 1))
    + rainWater1(0, vec.drop(split).reverse)

def rainWater1(mx: Int, xs: List[Int]): Int = xs match
  case x :: rest =>
    val newMx = x max mx
    newMx - x + rainWater1(newMx, rest)
  case _ =>
    0

def test(xs: Int*) =
  println(s"rainWater captured by ${xs.toList} = ${rainWater(xs.toList)}")

@main def Test =
  test(0,1,0,2,1,0,1,3,2,1,2,1)
