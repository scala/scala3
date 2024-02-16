import lib.InnerClass
import lib.InnerClassGen

@main def Test =

  locally:
    val ici: InnerClass = new InnerClass()
    val ici_inner1: ici.Inner[Long] = ici.createInner[Long](47L) // error

  locally:
    val ici: InnerClassGen[String] = new InnerClassGen()
    val ici_inner1: ici.Inner[Long] = ici.createInner[Long]("Hello", 47L) // error
