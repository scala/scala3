object a:
  val l = _root_.scala.language
  import l.noAutoTupling     // error
  import l.experimental.genericNumberLiterals  // error
  val scala = c
  import scala.language.noAutoTupling  // error // error
  val language = b
  import language.experimental.genericNumberLiterals // error

object b:
  val strictEquality = 22
  object experimental:
    val genericNumberLiterals = 22

object c:
  val language = b
  import b.strictEquality

object d:
  import language.experimental.genericNumberLiterals // ok
  import scala.language.noAutoTupling  // ok
  import _root_.scala.language.strictEquality // ok

