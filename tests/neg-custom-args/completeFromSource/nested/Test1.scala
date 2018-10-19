package completeFromSource.nested

class Test4 {

  val d = new D(1)
  val e = new E("xx")

  d.thisIsD(1)
  e.thisIsE() // error
}
