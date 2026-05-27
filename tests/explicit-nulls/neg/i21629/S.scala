import javax.annotation.J

class S {
  val j = new J

  // --- result types: @Nullable makes the return type T | Null ---

  def r1: String = j.returnNullable() // error
  def r2: String = J.staticReturnNullable() // error
  def r3: String = j.genericReturnNullable("x") // error
  def r4: Array[String] = j.returnNullableArray() // error
  def r5: java.util.List[String] = j.returnNullableGeneric("x") // error

  // ok: accepting T | Null
  def r1ok: String | Null = j.returnNullable()
  def r2ok: String | Null = J.staticReturnNullable()
  def r3ok: String | Null = j.genericReturnNullable("x")

  // --- argument types: @Nullable makes the parameter to T | Null ---

  def a1: String = j.argNullable(null) // ok: can pass null
  def a2: String = j.genericArgNullable(null) // ok: can pass null

  // --- type parameters: @Nullable inside generic makes element T | Null ---

  def t1: java.util.List[String] = j.returnListOfNullable() // error
  def t2: String = j.returnListOfNullable().get(0) // error
  def t3: java.util.Map[String, String] = j.returnMapWithNullableValue() // error

  // ok: accepting T | Null in the type argument
  def t1ok: java.util.List[String | Null] = j.returnListOfNullable()

  // --- fields: @Nullable makes the field type T | Null ---

  def f1: String = j.nullableField // error
  def f2: String = J.staticNullableField // error

  // ok
  def f1ok: String | Null = j.nullableField
  def f2ok: String | Null = J.staticNullableField
}
