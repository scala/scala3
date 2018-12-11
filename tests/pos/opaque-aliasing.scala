object Test {

  opaque type A = Int
  type AA = A

  var a: A = ???
  var aa: AA = ???

  object A {
    val x: A = a
    a = x
    val y: A = aa
    aa = y

    type BB = A
    val z1: BB = a
    val z2: BB = aa
    a = z1
    aa = z2
  }
}
object Test2 {

  opaque type A[X] = X
  type AA[X] = A[X]

  var a: A[Int] = ???
  var aa: AA[Int] = ???

  object A {
    val x: A[Int] = a
    a = x
    val y: A[Int] = aa
    aa = y

    type BB[X] = A[X]
    val z1: BB[Int] = a
    val z2: BB[Int] = aa
    a = z1
    aa = z2
  }
}