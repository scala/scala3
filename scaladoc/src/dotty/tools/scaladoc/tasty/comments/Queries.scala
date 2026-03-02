package dotty.tools.scaladoc
package tasty.comments

sealed trait Query {
  def asList: List[String] = this match {
    case Query.StrictMemberId(id) => id :: Nil
    case Query.Id(id) => id :: Nil
    case Query.QualifiedId(qual, _, rest) => qual.asString :: rest.asList
  }

  def join: String =
    def go(sb: StringBuilder, segment: Query): String = segment match {
      case Query.StrictMemberId(id) =>
        sb ++= id
        sb.toString
      case Query.Id(id) =>
        sb ++= id
        sb.toString
      case Query.QualifiedId(qual, sep, rest) =>
        sb ++= qual.asString
        sb += sep
        go(sb, rest)
    }
    go(new StringBuilder, this)
}

sealed trait QuerySegment extends Query
object Query {
  case class StrictMemberId(id: String) extends Query
  case class Id(id: String) extends QuerySegment
  case class QualifiedId(id: Qual, sep: Char, rest: QuerySegment) extends QuerySegment

  enum Qual {
    case Id(id: String)
    case This
    case Package

    def asString: String = this match {
      case Qual.This => "this"
      case Qual.Package => "package"
      case Qual.Id(id) => id
    }
  }
}

class QueryParser(val query: CharSequence) {
  private var idx = 0
  private var bld: StringBuilder = StringBuilder()

  def tryReadQuery(): Either[QueryParseException, Query] =
    try Right(readQuery()) catch {
      case ex : QueryParseException => Left(ex)
    }

  def readQuery(): Query = {
    assertBounds("expected start of query")
    if lookingAt('#') then {
      popCh()
      val res = readIdentifier().asString
      Query.StrictMemberId(res)
    } else readSegmentedQuery()
  }

  def readSegmentedQuery(): QuerySegment = {
    val id = readIdentifier()
    if atEnd() then {
      Query.Id(id.asString)
    } else if lookingAt('(') || lookingAt('[') then {
      // Include the entire signature in the identifier for overload resolution
      val signature = readSignature()
      Query.Id(id.asString + signature)
    } else {
      val ch = popCh()
      if ch == '.' || ch == '#'
      then Query.QualifiedId(id, ch, readSegmentedQuery())
      else err(s"expected . or #, instead saw: '$ch'")
    }
  }

  /** Read type parameters and parameter lists for overload resolution.
   *  This reads everything up to the next '.' or '#' or end of string.
   */
  private def readSignature(): String = {
    var depth = 0
    var result = new StringBuilder()
    while !atEnd() && (depth > 0 || !lookingAt('.') && !lookingAt('#')) do {
      val ch = popCh()
      result.append(ch)
      if ch == '[' || ch == '(' then depth += 1
      else if ch == ']' || ch == ')' then depth -= 1
    }
    result.toString
  }

  def readIdentifier(): Query.Qual = {
    assertBounds("expected start of identifier")
    if lookingAt('`') then {
      popCh()
      readQuotedIdentifier()
    } else readSimpleIdentifier()
  }

  def readSimpleIdentifier(): Query.Qual = {
    def atEndOfId(): Boolean = {
      var escaped = false
      atEnd() || {
        lookingAt('\\') && {
          popCh()
          escaped = true
          // NOTE: in principle we should never be at the end here, since
          // backslashes are always followed by another char. Ideally we'd just
          // throw an exception here, but that seems bad for someone who just
          // wants some documentation generated.
          atEnd()
        }
      } || {
        // NOTE: backquotes intentionally cannot be backslash-escaped, as they
        // cannot be used in Scala identifiers
        if lookingAt('`') then err("backquotes are not allowed in identifiers")
        if escaped then false else
          lookingAt('#') || lookingAt('.')
            || lookingAt('(') || lookingAt('[')
      }
    }

    while !atEndOfId() do pull()
    val res = popRes()
    if res.isEmpty then err("empty identifier")
    res match {
      case "this" => Query.Qual.This
      case "package" => Query.Qual.Package
      case res => Query.Qual.Id(res)
    }
  }

  def readQuotedIdentifier(): Query.Qual = {
    while {
      assertBounds("unexpected end of quoted identifier (expected '`')")
      !lookingAt('`')
    } do pull()
    popCh()
    val res = popRes()
    if res.isEmpty then err("empty quoted identifier")
    Query.Qual.Id(res)
  }

  private def popCh(): Char = {
    val res = query.charAt(idx)
    idx += 1
    res
  }

  private def popRes(): String = {
    val res = bld.toString
    bld = StringBuilder()
    res
  }

  private def pull(): Unit = {
    bld += query.charAt(idx)
    idx += 1
  }

  private def lookingAt(char: Char) = query.charAt(idx) == char

  private def atEnd() = idx >= query.length

  private def assertBounds(context: String) =
    if idx >= query.length then err(context)

  private def err(problem: String) =
    throw new QueryParseException(query, idx, problem)

  class QueryParseException(
    val query: CharSequence,
    val at: Int,
    val problem: String
  ) extends Exception(s"$problem at char $at in query: $query")
}
