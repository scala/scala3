class MyPrintStream extends java.io.PrintStream(??? : java.io.OutputStream):
  override def printf(format: String | Null, args: Array[? <: Object | Null])
  : java.io.PrintStream | Null = ???

class MyPrintStream2 extends java.io.PrintStream(??? : java.io.OutputStream):
  override def printf(format: String, args: Array[? <: Object])
  : java.io.PrintStream = ???
