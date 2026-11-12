import scala.language.strictEquality

def jdk(m: java.nio.file.AccessMode) = m match
  case java.nio.file.AccessMode.READ => 1
  case _ => 2

def local(c: JColor) = c match
  case JColor.RED => 1
  case _ => 2
