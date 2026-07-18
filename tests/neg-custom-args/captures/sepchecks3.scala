


def foo(xs: List[() => Unit], y: Object^) = ???

def bar(x: (Object^, Object^)): Unit = ???

def Test(c: Object^): Object^ = c // error

def Test2(consume c: Object^): Object^ = c // ok

def Test3(c: Object^): List[Object^] = c :: Nil // error
