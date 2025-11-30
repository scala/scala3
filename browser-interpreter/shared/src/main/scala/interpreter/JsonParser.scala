package interpreter

/**
 * Simple JSON parser for the AST format.
 * Works on both JVM and JavaScript.
 */
object JsonParser {
  import Ast._
  import Pattern._

  /**
   * Parse a JSON string into an AST.
   */
  def parse(json: String): Ast = {
    val value = parseJson(json)
    toAst(value)
  }

  /**
   * Parse JSON string to internal JSON representation.
   */
  private def parseJson(json: String): JsonValue = {
    var pos = 0

    def skipWhitespace(): Unit = {
      while (pos < json.length && json(pos).isWhitespace) pos += 1
    }

    def parseValue(): JsonValue = {
      skipWhitespace()
      if (pos >= json.length) throw new RuntimeException("Unexpected end of JSON")

      json(pos) match {
        case '{' => parseObject()
        case '[' => parseArray()
        case '"' => parseString()
        case 't' | 'f' => parseBoolean()
        case 'n' => parseNull()
        case c if c == '-' || c.isDigit => parseNumber()
        case c => throw new RuntimeException(s"Unexpected character: $c at position $pos")
      }
    }

    def parseObject(): JsonObject = {
      pos += 1 // skip '{'
      skipWhitespace()

      val fields = scala.collection.mutable.Map[String, JsonValue]()

      while (pos < json.length && json(pos) != '}') {
        skipWhitespace()
        if (json(pos) == ',') pos += 1
        skipWhitespace()
        if (json(pos) == '}') {} // do nothing, will exit loop
        else {
          val key = parseString().value
          skipWhitespace()
          if (json(pos) != ':') throw new RuntimeException(s"Expected ':' at position $pos")
          pos += 1
          skipWhitespace()
          val value = parseValue()
          fields(key) = value
          skipWhitespace()
        }
      }

      if (pos >= json.length || json(pos) != '}') throw new RuntimeException(s"Expected '}' at position $pos")
      pos += 1

      JsonObject(fields.toMap)
    }

    def parseArray(): JsonArray = {
      pos += 1 // skip '['
      skipWhitespace()

      val elements = scala.collection.mutable.ListBuffer[JsonValue]()

      while (pos < json.length && json(pos) != ']') {
        skipWhitespace()
        if (json(pos) == ',') pos += 1
        skipWhitespace()
        if (json(pos) == ']') {} // do nothing, will exit loop
        else {
          elements += parseValue()
          skipWhitespace()
        }
      }

      if (pos >= json.length || json(pos) != ']') throw new RuntimeException(s"Expected ']' at position $pos")
      pos += 1

      JsonArray(elements.toList)
    }

    def parseString(): JsonString = {
      pos += 1 // skip opening quote
      val sb = new StringBuilder()

      while (pos < json.length && json(pos) != '"') {
        if (json(pos) == '\\') {
          pos += 1
          json(pos) match {
            case '"' => sb += '"'
            case '\\' => sb += '\\'
            case '/' => sb += '/'
            case 'b' => sb += '\b'
            case 'f' => sb += '\f'
            case 'n' => sb += '\n'
            case 'r' => sb += '\r'
            case 't' => sb += '\t'
            case 'u' =>
              val hex = json.substring(pos + 1, pos + 5)
              sb += Integer.parseInt(hex, 16).toChar
              pos += 4
            case c => sb += c
          }
        } else {
          sb += json(pos)
        }
        pos += 1
      }

      pos += 1 // skip closing quote
      JsonString(sb.toString)
    }

    def parseNumber(): JsonNumber = {
      val start = pos
      if (json(pos) == '-') pos += 1
      while (pos < json.length && json(pos).isDigit) pos += 1
      if (pos < json.length && json(pos) == '.') {
        pos += 1
        while (pos < json.length && json(pos).isDigit) pos += 1
      }
      if (pos < json.length && (json(pos) == 'e' || json(pos) == 'E')) {
        pos += 1
        if (json(pos) == '+' || json(pos) == '-') pos += 1
        while (pos < json.length && json(pos).isDigit) pos += 1
      }
      JsonNumber(json.substring(start, pos).toDouble)
    }

    def parseBoolean(): JsonBoolean = {
      if (json.substring(pos).startsWith("true")) {
        pos += 4
        JsonBoolean(true)
      } else if (json.substring(pos).startsWith("false")) {
        pos += 5
        JsonBoolean(false)
      } else {
        throw new RuntimeException(s"Expected boolean at position $pos")
      }
    }

    def parseNull(): JsonNull.type = {
      if (json.substring(pos).startsWith("null")) {
        pos += 4
        JsonNull
      } else {
        throw new RuntimeException(s"Expected null at position $pos")
      }
    }

    parseValue()
  }

  // Internal JSON representation
  sealed trait JsonValue
  case class JsonObject(fields: Map[String, JsonValue]) extends JsonValue
  case class JsonArray(elements: List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  /**
   * Convert JSON to AST.
   */
  private def toAst(json: JsonValue): Ast = {
    json match {
      case obj: JsonObject =>
        val tag = getString(obj, "tag")
        tag match {
          case "Literal" =>
            val tpe = obj.fields.get("type").map(getString(_, "")).getOrElse("Unknown")
            val value = obj.fields.get("value")
            tpe match {
              case "Int" => IntLit(getNumber(value.get).toInt)
              case "Long" => LongLit(getNumber(value.get).toLong)
              case "Double" => DoubleLit(getNumber(value.get))
              case "Float" => FloatLit(getNumber(value.get).toFloat)
              case "Boolean" => BoolLit(getBoolean(value.get))
              case "String" => StringLit(getString(value.get, ""))
              case "Char" => CharLit(getString(value.get, "").headOption.getOrElse(' '))
              case "Unit" => UnitLit
              case "Null" => NullLit
              case _ =>
                // Infer from value
                value match {
                  case Some(JsonNumber(n)) =>
                    if (n == n.toInt) IntLit(n.toInt) else DoubleLit(n)
                  case Some(JsonBoolean(b)) => BoolLit(b)
                  case Some(JsonString(s)) => StringLit(s)
                  case Some(JsonNull) | None => NullLit
                  case _ => UnitLit
                }
            }

          case "Ident" =>
            Ident(getString(obj, "name"))

          case "Select" =>
            Select(toAst(getObj(obj, "receiver")), getString(obj, "name"))

          case "Block" =>
            val stats = getArray(obj, "stats").map(toAst)
            val expr = toAst(getObj(obj, "expr"))
            Block(stats, expr)

          case "If" =>
            If(toAst(getObj(obj, "cond")), toAst(getObj(obj, "thenp")), toAst(getObj(obj, "elsep")))

          case "While" =>
            While(toAst(getObj(obj, "cond")), toAst(getObj(obj, "body")))

          case "Match" =>
            val selector = toAst(getObj(obj, "selector"))
            val cases = getArray(obj, "cases").map(toCaseDef)
            Match(selector, cases)

          case "Try" =>
            val block = toAst(getObj(obj, "block"))
            val catches = getArrayOpt(obj, "catches").map(toCaseDef)
            val finalizer = obj.fields.get("finalizer").map(toAst)
            Try(block, catches, finalizer)

          case "Return" =>
            Return(toAst(getObj(obj, "expr")))

          case "Throw" =>
            Throw(toAst(getObj(obj, "expr")))

          case "BinaryOp" =>
            BinaryOp(getString(obj, "op"), toAst(getObj(obj, "lhs")), toAst(getObj(obj, "rhs")))

          case "UnaryOp" =>
            UnaryOp(getString(obj, "op"), toAst(getObj(obj, "arg")))

          case "Apply" =>
            val fn = toAst(getObj(obj, "fn"))
            val args = getArray(obj, "args").map(toAst)
            Apply(fn, args)

          case "New" =>
            val className = getString(obj, "class")
            val args = getArrayOpt(obj, "args").map(toAst)
            New(className, args)

          case "Assign" =>
            Assign(getString(obj, "name"), toAst(getObj(obj, "rhs")))

          case "ValDef" =>
            ValDef(getString(obj, "name"), toAst(getObj(obj, "rhs")), mutable = false)

          case "VarDef" =>
            ValDef(getString(obj, "name"), toAst(getObj(obj, "rhs")), mutable = true)

          case "DefDef" =>
            val name = getString(obj, "name")
            val params = getStringArray(obj, "params")
            val body = toAst(getObj(obj, "body"))
            DefDef(name, params, body)

          case "Lambda" | "Closure" =>
            val params = getStringArray(obj, "params")
            val body = toAst(getObj(obj, "body"))
            Lambda(params, body)

          case _ =>
            throw new RuntimeException(s"Unknown AST tag: $tag")
        }

      case _ =>
        throw new RuntimeException(s"Expected object, got ${json.getClass.getSimpleName}")
    }
  }

  /**
   * Convert JSON to CaseDef.
   */
  private def toCaseDef(json: JsonValue): CaseDef = {
    val obj = json.asInstanceOf[JsonObject]
    val pattern = toPattern(getObj(obj, "pattern"))
    val guard = obj.fields.get("guard").map(toAst)
    val body = toAst(getObj(obj, "body"))
    CaseDef(pattern, guard, body)
  }

  /**
   * Convert JSON to Pattern.
   */
  private def toPattern(json: JsonValue): Pattern = {
    val obj = json.asInstanceOf[JsonObject]
    val tag = getString(obj, "tag")

    tag match {
      case "Wildcard" => Wildcard

      case "Bind" =>
        val name = getString(obj, "name")
        val inner = obj.fields.get("inner").map(toPattern)
        Bind(name, inner)

      case "Literal" =>
        val value = obj.fields.get("value")
        value match {
          case Some(JsonNumber(n)) =>
            if (n == n.toInt) Pattern.Literal(n.toInt) else Pattern.Literal(n)
          case Some(JsonBoolean(b)) => Pattern.Literal(b)
          case Some(JsonString(s)) => Pattern.Literal(s)
          case _ => Pattern.Literal(null)
        }

      case "Typed" =>
        val tpe = getString(obj, "type")
        val inner = obj.fields.get("inner").map(toPattern)
        Typed(tpe, inner)

      case "Unapply" =>
        val className = getString(obj, "class")
        val patterns = getArrayOpt(obj, "patterns").map(toPattern)
        Unapply(className, patterns)

      case "Alternative" =>
        val patterns = getArray(obj, "patterns").map(toPattern)
        Alternative(patterns)

      case _ =>
        Wildcard // Default to wildcard for unknown patterns
    }
  }

  // Helper methods
  private def getString(obj: JsonObject, key: String): String = {
    obj.fields.get(key) match {
      case Some(JsonString(s)) => s
      case _ => ""
    }
  }

  private def getString(json: JsonValue, default: String): String = {
    json match {
      case JsonString(s) => s
      case _ => default
    }
  }

  private def getNumber(json: JsonValue): Double = {
    json match {
      case JsonNumber(n) => n
      case JsonString(s) => s.toDouble
      case _ => 0.0
    }
  }

  private def getBoolean(json: JsonValue): Boolean = {
    json match {
      case JsonBoolean(b) => b
      case _ => false
    }
  }

  private def getObj(obj: JsonObject, key: String): JsonValue = {
    obj.fields.getOrElse(key, throw new RuntimeException(s"Missing key: $key"))
  }

  private def getArray(obj: JsonObject, key: String): List[JsonValue] = {
    obj.fields.get(key) match {
      case Some(JsonArray(elements)) => elements
      case _ => Nil
    }
  }

  private def getArrayOpt(obj: JsonObject, key: String): List[JsonValue] = {
    obj.fields.get(key) match {
      case Some(JsonArray(elements)) => elements
      case _ => Nil
    }
  }

  private def getStringArray(obj: JsonObject, key: String): List[String] = {
    obj.fields.get(key) match {
      case Some(JsonArray(elements)) => elements.map {
        case JsonString(s) => s
        case _ => ""
      }
      case _ => Nil
    }
  }

}

