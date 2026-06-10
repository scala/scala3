// Test case for TermRef handling in jsig
// jsig matches: case ref: TermRef if ref.symbol.isGetter

class TermRefTest:
  // Getter for a val
  val myValue: Int = 42
  
  def useValRef(): Int = myValue

  // Getter for a property with by-name parameter
  def byNameGetter(f: => Int): Int = f

  def resultOfByName(): Int = byNameGetter(myValue)

  // Val with reference type
  val stringVal: String = "test"

  def refString(): String = stringVal

  // Multiple vals creating term references
  val a: Int = 1
  val b: Int = 2

  def sumVals(): Int = a + b

  // Val with generic type
  val genericVal: List[Int] = List(1, 2, 3)

  def useGenericVal(): List[Int] = genericVal

  // Val used in function type
  def functionFromVal(): () => Int = () => myValue

  // Parameter that becomes a getter
  class WithParam(val param: String):
    def getParam: String = param
