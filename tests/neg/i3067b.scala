import collection.generic.CanBuildFrom

class Test[T](f: List[String] => T)

object o {

  implicitly[CanBuildFrom[String, Char, String]]

  implicit object b extends Test(_ map identity)  // error: type needs to be given // error: cyclic reference

}
