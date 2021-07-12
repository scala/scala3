case class Wrapper[A <: Singleton](a: A)(using ValueOf[A])

def test[A <: Singleton](blabla: Wrapper[A]): Unit = ()

val _ = test(Wrapper("yo"))