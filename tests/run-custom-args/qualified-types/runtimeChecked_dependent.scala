def foo(x: Int, y: {v: Int with v > x}): y.type = y

def getInt(): Int = 1

type Pos = {v: Int with v > 0}
type Neg = {v: Int with v < 0}

import scala.reflect.TypeTest

@main def Test =
  val v1= foo(1, 2.runtimeChecked)

  val p: Int = 1
  val v2 = foo(p, 2.runtimeChecked)
