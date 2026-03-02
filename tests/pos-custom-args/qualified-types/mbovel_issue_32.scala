// Regression test for https://github.com/mbovel/dotty/issues/32

def foo[T <: Int, Q <: {t: T with t > 0}]: Q = ???
