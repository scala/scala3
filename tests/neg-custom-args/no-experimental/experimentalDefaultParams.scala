import scala.annotation.experimental

@experimental def x = 2

def test1(
  p6: Any = x // error: def x is marked @experimental and therefore ...
): Any = ???

@experimental def test2(
  p6: Any = x
): Any = ???

class Test1(
  p6: Any = x // error
) {}

@experimental class Test2(
  p6: Any = x
) {}

trait Test3(
  p6: Any = x // error
) {}

@experimental trait Test4(
  p6: Any = x
) {}
