import scala.language.experimental.subCases

object Test:

  // using transparent to test whether test whether reduced as expected
  transparent inline def foo(i: Int, j: Int): String =
    inline i match
      case 0 if j match
        case 1 => "01"
        case 2 => "02"
      case 1 if j match
        case 1 => "11"
        case 2 => "12"
      case _ => "3"

  val r01: "01" = foo(0, 1)
  val r02: "02" = foo(0, 2)
  val r11: "11" = foo(1, 1)
  val r31: "3" = foo(3, 1)

  transparent inline def bar(x: Option[Any]): String =
    inline x match
      case Some(y: Int) if y match
        case 1 => "a"
        case 2 => "b"
      case Some(z: String) => "c"
      case _ => "d"

  val x = bar(Some(2)) // FIX this reduces to "c", but this appears to be a more general issue than sub-matches
  val y = bar(Some("hello"))
