class X[T] { trait Id }
object A extends X[B]
class B(id: A.Id)
