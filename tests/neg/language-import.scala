object a with
  val l = _root_.scala.language
  import l.noAutoTupling     // error
  import l.experimental.genericNumberLiterals  // error
  val scala = c
  import scala.language.noAutoTupling  // error
  val language = b
  import language.experimental.genericNumberLiterals // error

object b with
  val strictEquality = 22
  object experimental with
    val genericNumberLiterals = 22

object c with
  val language = b
  import b.strictEquality

object d with
  import language.experimental.genericNumberLiterals // ok
  import scala.language.noAutoTupling  // ok
  import _root_.scala.language.strictEquality // ok

