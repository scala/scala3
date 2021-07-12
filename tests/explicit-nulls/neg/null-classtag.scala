def f = {
  val a: Array[Null] = Array(null) // error: No ClassTag available for Null
}

def g = {
  import scala.language.unsafeNulls
  val a: Array[Null] = Array(null) // error: No ClassTag available for Null
}