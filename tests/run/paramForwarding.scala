// scalajs: --skip

// A contains a field A.theValue$$local accessible using the
// generated getter A.theValue()
class A(val theValue: Int) {
  val theValueInA = theValue // use the constructor parameter theValue

  def getTheValue = theValue // virtual call to the getter theValue()
}

// B contains a field B.theValue$$local accessible using the getter
// B.theValue() which overrides A.theValue()
class B(override val theValue: Int) extends A(42) {
  val theValueInB = theValue
}

// Bz contains a field Bz.theValue$$local accessible using the getter
// Bz.theValue() which overrides A.theValue()
class Bz extends A(42) {
  override val theValue: Int = 10
  val theValueInBz = theValue
}

// C does contain a field C.theValue$$local
class C(override val theValue: Int) extends A(theValue)

// D contains a field D.other$$local and a corresponding getter.
class D(val other: Int) extends A(other)


// NonVal does not contain a field NonVal.theValue$$local.
class NonVal(theValue: Int) extends A(theValue) {
  def getTheValueInNonVal = theValue // use the constructor parameter theValue
}

// X contains a field X.theValue$$local accessible using the getter
// X.theValue() which overrides A.theValue()
class X(override val theValue: Int) extends NonVal(0)

// Y does contain a field Y.theValue$$local
class Y(override val theValue: Int) extends NonVal(theValue)


object Test {
  def printFields(obj: Any) =
    println(obj.getClass.getDeclaredFields.map(_.toString).sorted.toList.mkString("\n"))

  def main(args: Array[String]): Unit = {
    val b10 = new B(10)
    val bz = new Bz
    val c11 = new C(11)
    val d12 = new D(12)
    val nv13 = new NonVal(13)
    val x14 = new X(14)
    val y15 = new Y(15)

    println("B:")
    printFields(b10)
    println("Bz:")
    printFields(bz)
    println("C:")
    printFields(c11)
    println("D:")
    printFields(d12)
    println("NonVal:")
    printFields(nv13)
    println("X:")
    printFields(x14)
    println("Y:")
    printFields(y15)


    assert(b10.getTheValue == 10)
    assert(b10.theValue == 10)
    assert(b10.theValueInB == 10)
    assert(b10.theValueInA == 42)

    assert(bz.getTheValue == 10)
    assert(bz.theValue == 10)
    assert(bz.theValueInBz == 10)
    assert(bz.theValueInA == 42)


    assert(x14.theValue == 14)
    assert(x14.getTheValue == 14)
    assert(x14.getTheValueInNonVal == 0)
    assert(x14.theValueInA == 0)
  }
}
