//> using options -Werror

def foo(x: Int) = x

def test = foo _ // error
