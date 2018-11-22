
class S {
  implicit def cat2Dog(cat: Cat): Dog = ???
  
  val j = new J()
  // j.getCat() returns a Cat|JavaNull, so we look for implicit conversions
  // from Cat to Dog
  bark(j.getCat())

  // We can't find the implicit conversion, because `Null` (as opposed to `JavaNull`)
  // isn't stripped.
  def getCat2: Cat|Null = null
  bark(getCat2) // error
  
  def bark(dog: Dog) = {}
}
