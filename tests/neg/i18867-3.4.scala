//> using options -Werror

import language.`3.4`

def foo(x: Int) = x

def test = foo _ // error
