def g[@inline x: _](y: x): Unit = () // error: Unbound wildcard type

trait F[A]
trait G[A]
def ok[A: {F, G}](a: A): Unit = () // pos is start of A | F
def f0[A: {F, _}](a: A): Unit = () // error
def f1[A: {_, G}](a: A): Unit = () // error
def f2[A: {_, _}](a: A): Unit = () // error // error
