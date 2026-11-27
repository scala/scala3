import scala.scalajs.wasm
import scala.scalajs.wasm.annotation.*

object Singletons {
  val single: Array[Int] = new Array[Int](1)
}

object ImportParams {
  @WasmImport("env", "testOK")
  def testOK(x: Int): Unit = wasm.native // ok

  @WasmImport("env", "testByte")
  def testByte(x: Byte): Unit = wasm.native // error
  @WasmImport("env", "testShort")
  def testShort(x: Short): Unit = wasm.native // error
  @WasmImport("env", "testBoolean")
  def testBoolean(x: Boolean): Unit = wasm.native // error
  @WasmImport("env", "testString")
  def testString(x: String): Unit = wasm.native // error
  @WasmImport("env", "testChar")
  def testChar(x: Char): Unit = wasm.native // error
  @WasmImport("env", "testArrayBoolean")
  def testArrayBoolean(x: Array[Boolean]): Unit = wasm.native // error
  @WasmImport("env", "testArrayChar")
  def testArrayChar(x: Array[Char]): Unit = wasm.native // error
  @WasmImport("env", "testArrayString")
  def testArrayString(x: Array[String]): Unit = wasm.native // error
  @WasmImport("env", "testArrayArrayInt")
  def testArrayArrayInt(x: Array[Array[Int]]): Unit = wasm.native // error
  @WasmImport("env", "testAny")
  def testAny(x: Any): Unit = wasm.native // error
  @WasmImport("env", "testAnyRef")
  def testAnyRef(x: AnyRef): Unit = wasm.native // error
  @WasmImport("env", "testNothing")
  def testNothing(x: Nothing): Unit = wasm.native // error
  @WasmImport("env", "testNull")
  def testNull(x: Null): Unit = wasm.native // error
  @WasmImport("env", "testJLInteger")
  def testJLInteger(x: java.lang.Integer): Unit = wasm.native // error
  @WasmImport("env", "testArrayJLInteger")
  def testArrayJLInteger(x: Array[java.lang.Integer]): Unit = wasm.native // error
  @WasmImport("env", "testListInt")
  def testListInt(x: List[Int]): Unit = wasm.native // error
  @WasmImport("env", "testSingleton")
  def testSingleton(x: Singletons.single.type): Unit = wasm.native // error
  @WasmImport("env", "testUnit")
  def testUnit(x: Unit): Unit = wasm.native // error
}

object ImportResults {
  @WasmImport("env", "testOK")
  def testOK(): Int = wasm.native // ok

  @WasmImport("env", "testByte")
  def testByte(): Byte = wasm.native // error
  @WasmImport("env", "testShort")
  def testShort(): Short = wasm.native // error
  @WasmImport("env", "testBoolean")
  def testBoolean(): Boolean = wasm.native // error
  @WasmImport("env", "testString")
  def testString(): String = wasm.native // error
  @WasmImport("env", "testChar")
  def testChar(): Char = wasm.native // error
  @WasmImport("env", "testArrayBoolean")
  def testArrayBoolean(): Array[Boolean] = wasm.native // error
  @WasmImport("env", "testArrayChar")
  def testArrayChar(): Array[Char] = wasm.native // error
  @WasmImport("env", "testArrayString")
  def testArrayString(): Array[String] = wasm.native // error
  @WasmImport("env", "testArrayArrayInt")
  def testArrayArrayInt(): Array[Array[Int]] = wasm.native // error
  @WasmImport("env", "testAny")
  def testAny(): Any = wasm.native // error
  @WasmImport("env", "testAnyRef")
  def testAnyRef(): AnyRef = wasm.native // error
  @WasmImport("env", "testNothing")
  def testNothing(): Nothing = wasm.native // error
  @WasmImport("env", "testNull")
  def testNull(): Null = wasm.native // error
  @WasmImport("env", "testJLInteger")
  def testJLInteger(): java.lang.Integer = wasm.native // error
  @WasmImport("env", "testArrayJLInteger")
  def testArrayJLInteger(): Array[java.lang.Integer] = wasm.native // error
  @WasmImport("env", "testListInt")
  def testListInt(): List[Int] = wasm.native // error
  @WasmImport("env", "testSingleton")
  def testSingleton(): Singletons.single.type = wasm.native // error
}

object ExportParams {
  @WasmExport("testOK")
  def testOK(x: Int): Unit = ??? // ok

  @WasmExport("testByte")
  def testByte(x: Byte): Unit = ??? // error
  @WasmExport("testShort")
  def testShort(x: Short): Unit = ??? // error
  @WasmExport("testBoolean")
  def testBoolean(x: Boolean): Unit = ??? // error
  @WasmExport("testString")
  def testString(x: String): Unit = ??? // error
  @WasmExport("testChar")
  def testChar(x: Char): Unit = ??? // error
  @WasmExport("testArrayBoolean")
  def testArrayBoolean(x: Array[Boolean]): Unit = ??? // error
  @WasmExport("testArrayChar")
  def testArrayChar(x: Array[Char]): Unit = ??? // error
  @WasmExport("testArrayString")
  def testArrayString(x: Array[String]): Unit = ??? // error
  @WasmExport("testArrayArrayInt")
  def testArrayArrayInt(x: Array[Array[Int]]): Unit = ??? // error
  @WasmExport("testAny")
  def testAny(x: Any): Unit = ??? // error
  @WasmExport("testAnyRef")
  def testAnyRef(x: AnyRef): Unit = ??? // error
  @WasmExport("testNothing")
  def testNothing(x: Nothing): Unit = ??? // error
  @WasmExport("testNull")
  def testNull(x: Null): Unit = ??? // error
  @WasmExport("testJLInteger")
  def testJLInteger(x: java.lang.Integer): Unit = ??? // error
  @WasmExport("testArrayJLInteger")
  def testArrayJLInteger(x: Array[java.lang.Integer]): Unit = ??? // error
  @WasmExport("testListInt")
  def testListInt(x: List[Int]): Unit = ??? // error
  @WasmExport("testSingleton")
  def testSingleton(x: Singletons.single.type): Unit = ??? // error
  @WasmExport("testUnit")
  def testUnit(x: Unit): Unit = ??? // error
}

object ExportResults {
  @WasmExport("testOK")
  def testOK(): Int = ??? // ok

  @WasmExport("testByte")
  def testByte(): Byte = ??? // error
  @WasmExport("testShort")
  def testShort(): Short = ??? // error
  @WasmExport("testBoolean")
  def testBoolean(): Boolean = ??? // error
  @WasmExport("testString")
  def testString(): String = ??? // error
  @WasmExport("testChar")
  def testChar(): Char = ??? // error
  @WasmExport("testArrayBoolean")
  def testArrayBoolean(): Array[Boolean] = ??? // error
  @WasmExport("testArrayChar")
  def testArrayChar(): Array[Char] = ??? // error
  @WasmExport("testArrayString")
  def testArrayString(): Array[String] = ??? // error
  @WasmExport("testArrayArrayInt")
  def testArrayArrayInt(): Array[Array[Int]] = ??? // error
  @WasmExport("testAny")
  def testAny(): Any = ??? // error
  @WasmExport("testAnyRef")
  def testAnyRef(): AnyRef = ??? // error
  @WasmExport("testNothing")
  def testNothing(): Nothing = ??? // error
  @WasmExport("testNull")
  def testNull(): Null = ??? // error
  @WasmExport("testJLInteger")
  def testJLInteger(): java.lang.Integer = ??? // error
  @WasmExport("testArrayJLInteger")
  def testArrayJLInteger(): Array[java.lang.Integer] = ??? // error
  @WasmExport("testListInt")
  def testListInt(): List[Int] = ??? // error
  @WasmExport("testSingleton")
  def testSingleton(): Singletons.single.type = ??? // error
}
