object StatusCode:
  enum Matcher:
    case ServerError extends Matcher
  end Matcher
end StatusCode

enum StatusCode(code: Int, m: StatusCode.Matcher):
  case InternalServerError extends StatusCode(500, StatusCode.Matcher.ServerError)
end StatusCode

object Main {
  def main(args: Array[String]): Unit = {
    println(StatusCode.InternalServerError)
  }
}
