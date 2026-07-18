import scala.language.experimental.subCases

val pf: PartialFunction[Option[Option[Int]], String] =
  case Some(x2) if x2 match
    case Some(x3) if x3 match
      case 1 => "a"
      case 2 => "b"
  case Some(None) => "c"

@main def Test =
  assert(pf(Some(Some(2))) == "b")
  assert(pf(Some(None)) == "c")
  assert(!pf.isDefinedAt(None))
  assert(!pf.isDefinedAt(Some(Some(3))))
