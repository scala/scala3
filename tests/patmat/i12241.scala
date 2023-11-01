sealed trait EndpointInput[T]

object EndpointInput {
  case class Pair[T]() extends EndpointInput[T]
  case class MappedPair[T]() extends EndpointInput[T]
  case class Pair2[T]() extends EndpointInput[T]
  case class MappedPair2[T]() extends EndpointInput[T]
  case class FixedMethod[T]() extends EndpointInput[T]
  case class FixedPath[T]() extends EndpointInput[T]
  case class PathCapture[T]() extends EndpointInput[T]
  case class PathsCapture[T]() extends EndpointInput[T]
  case class Query[T]() extends EndpointInput[T]
  case class QueryParams[T]() extends EndpointInput[T]
  case class Cookie[T]() extends EndpointInput[T]
  case class ExtractFromRequest[T]() extends EndpointInput[T]
  case class ApiKey[T]() extends EndpointInput[T]
  case class Http[T]() extends EndpointInput[T]
  case class Body[R, T]() extends EndpointInput[T]
  case class FixedHeader[T]() extends EndpointInput[T]
  case class Header[T]() extends EndpointInput[T]
  case class Headers[T]() extends EndpointInput[T]
  case class StatusCode[T]() extends EndpointInput[T]
  case class Empty[T]() extends EndpointInput[T]
}

object Test extends App {
  import EndpointInput._

  def compare(left: EndpointInput[?], right: EndpointInput[?]): Boolean =
    (left, right) match {
      case (Pair(), Pair()) => true
      case (MappedPair(), MappedPair()) => true
      case (Pair2(), Pair2()) => true
      case (MappedPair2(), MappedPair2()) => true
      case (FixedMethod(), FixedMethod()) => true
      case (FixedPath(), FixedPath()) => true
      case (PathCapture(), PathCapture()) => true
      case (PathsCapture(), PathsCapture()) => true
      case (Query(), Query()) => true
      case (QueryParams(), QueryParams()) => true
      case (Cookie(), Cookie()) => true
      case (ExtractFromRequest(), ExtractFromRequest()) => true
      case (ApiKey(), ApiKey()) => true
      case (Http(), Http()) => true
      case (Body(), Body()) => true
      case (FixedHeader(), FixedHeader()) => true
      case (Header(), Header()) => true
      case (Headers(), Headers()) => true
      case (StatusCode(), StatusCode()) => true
      case (_, _) => false
    }

  def compare2(left: EndpointInput[?], right: EndpointInput[?]): Boolean =
    (left, right) match {
      case (Pair(), Pair()) => true
      case (MappedPair(), MappedPair()) => true
      case (Pair2(), Pair2()) => true
      case (MappedPair2(), MappedPair2()) => true
      case (FixedMethod(), FixedMethod()) => true
      case (FixedPath(), FixedPath()) => true
      case (PathCapture(), PathCapture()) => true
      case (PathsCapture(), PathsCapture()) => true
      case (Query(), Query()) => true
      case (QueryParams(), QueryParams()) => true
      case (Cookie(), Cookie()) => true
      case (ExtractFromRequest(), ExtractFromRequest()) => true
      case (ApiKey(), ApiKey()) => true
      case (Http(), Http()) => true
      case (Body(), Body()) => true
      case (FixedHeader(), FixedHeader()) => true
      case (Header(), Header()) => true
      case (Headers(), Headers()) => true
      case (StatusCode(), StatusCode()) => true
    }
}
