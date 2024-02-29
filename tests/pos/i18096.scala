trait F1[-T1, +R] extends AnyRef { def apply(v1: T1): R }
class R { def l: List[Any] = Nil }
class S { def m[T](f: F1[R, ? <: List[T]]): S = this }
class T1 { def t1(s: S) = s.m((r: R) => r.l) }
