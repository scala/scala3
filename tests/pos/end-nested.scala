def f[T](x: Option[T]) = x match
  case Some(y) =>
  case None =>
end f

object Test with
  try List(1, 2, 3) match
  case x :: xs => println(x)
  case Nil => println("Nil")
  catch
  case ex: java.io.IOException => println(ex)
  case ex: Throwable => throw ex
  end try

