package givens

trait X:
  def doX: Int

trait Y:
  def doY: String

trait Z[T]:
  def doZ: List[T]



given intValue: Int = 4
given String = "str"
given (using Int): Double = 4.0
given [T]: List[T] = Nil
given given_Char: Char = '?'
given `given_Float`: Float = 3.0
given `* *`: Long = 5

given X with
  def doX = 7

given (using X): Y with
  def doY = "7"

given [T]: Z[T] with
  def doZ: List[T] = Nil



val a = intValue
val b = given_String
val c = given_Double
val d = given_List_T[Int]
val e = given_Char
val f = given_Float
val g = `* *`
val x = given_X
val y = given_Y
val z = given_Z_T[String]
