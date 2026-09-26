final class Token

private val expected = new Token
private var events = List.empty[String]

def makeToken(): Token =
  events :+= "token"
  expected

def next(index: Int): Int =
  events :+= s"arg$index"
  index

def keep(token: Token)(xs: Int*): token.type = token

@main def Test =
  val token = keep(makeToken())(next(1), next(2))
  assert(token eq expected)
  assert(events == List("token", "arg1", "arg2"), events)
