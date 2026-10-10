//> using options -Ytest-pickler-check

// TastyPrinter must read signed constant payloads with readInt/readLongInt
class A:
  final val b: Byte = -1
  final val s: Short = -1
  final val c = 'x'
  final val i = -1
  final val ih = 0xcafebabe
  final val l = Long.MinValue
  final val f = -1.0f
  final val d = -1.0
