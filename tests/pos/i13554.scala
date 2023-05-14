object StatusCode:
  class Matcher

enum StatusCode(m: StatusCode.Matcher):
  case InternalServerError extends StatusCode(???)

