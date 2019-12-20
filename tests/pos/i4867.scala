object Main_i4867 {

  given extension [AB](ab: AB) {
    def id: AB = ab
  }

  def main(args : Array[String]) : Unit = {
    val int : Int = 2
    val foundInt : Int = int.id

    val stringOrInt : String | Int = 1
    val foundAny : String | Int = stringOrInt.id
  }

}
