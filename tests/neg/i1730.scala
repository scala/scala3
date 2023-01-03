import scala.reflect.ClassTag

@main def Test =
  val x: Array[? <: String] = Array[Int & Nothing]() // error: No ClassTag available for Int & Nothing
                                                     // (was: ClassCastException: [I cannot be cast to [Ljava.lang.String)
  val y: Array[? <: Int] = Array[String & Nothing]() // error: No ClassTag available for String & Nothing
                                                     // (was: ClassCastException: [Lscala.runtime.Nothing$; cannot be cast to [I)
