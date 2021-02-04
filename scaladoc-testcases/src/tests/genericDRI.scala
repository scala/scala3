package tests.genericDRI

class A
class B

def a[T <: A](t: T) = 1
def a[T <: B](t: T) = 2

def a[T >: A](t: T) = 3
