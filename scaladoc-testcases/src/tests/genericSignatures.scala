package tests.genericSignatures

class D[T]

// TODO #26 this is not supported :( class D1[T ]

class D2[T]()

class DD[+T]

class A

class E[T] extends D[T]

class F[+T, -F] extends DD[T]

class A2[E, +T <: DD[E]]
