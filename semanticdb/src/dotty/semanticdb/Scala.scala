package dotty.semanticdb

import scala.compat.Platform.EOL
import dotty.semanticdb.Scala.{Descriptor => d}
import dotty.semanticdb.Scala.{Names => n}

object Scala {
  object Symbols {
    val None: String = ""
    val RootPackage: String = "_root_/"
    val EmptyPackage: String = "_empty_/"
    def Global(owner: String, desc: Descriptor): String =
      if (owner != RootPackage) owner + desc.toString
      else desc.toString
    def Local(suffix: String): String = {
      if (suffix.indexOf("/") == -1 && suffix.indexOf(";") == -1) "local" + suffix
      else throw new IllegalArgumentException(suffix)
    }
    def Multi(symbols: List[String]): String = {
      symbols.distinct match {
        case List(symbol) =>
          symbol
        case symbols =>
          val sb = new StringBuilder
          symbols.foreach { symbol =>
            if (!symbol.isMulti) {
              sb.append(';')
            }
            sb.append(symbol)
          }
          sb.toString()
      }
    }
  }

  implicit class ScalaSymbolOps(symbol: String) {
    def isNone: Boolean =
      symbol == Symbols.None
    def isRootPackage: Boolean =
      symbol == Symbols.RootPackage
    def isEmptyPackage: Boolean =
      symbol == Symbols.EmptyPackage
    def isGlobal: Boolean =
      !isNone && !isMulti && (symbol.last match {
        case '.' | '#' | '/' | ')' | ']' => true
        case _ => false
      })
    def isLocal: Boolean =
      symbol.startsWith("local")
    def isMulti: Boolean =
      symbol.startsWith(";")
    def asMulti: List[String] = {
      if (!isMulti) symbol :: Nil
      else {
        val buf = List.newBuilder[String]
        def loop(begin: Int, i: Int): Unit =
          if (i >= symbol.length) {
            buf += symbol.substring(begin, symbol.length)
          } else {
            symbol.charAt(i) match {
              case ';' =>
                buf += symbol.substring(begin, i)
                loop(i + 1, i + 1)
              case '`' =>
                var j = i + 1
                while (symbol.charAt(j) != '`') j += 1
                loop(begin, j + 1)
              case _ =>
                loop(begin, i + 1)
            }
          }
        loop(1, 1)
        buf.result()
      }
    }
    def isTerm: Boolean =
      !isNone && !isMulti && symbol.last == '.'
    def isType: Boolean =
      !isNone && !isMulti && symbol.last == '#'
    def isPackage: Boolean =
      !isNone && !isMulti && symbol.last == '/'
    def isParameter: Boolean =
      !isNone && !isMulti && symbol.last == ')'
    def isTypeParameter: Boolean =
      !isNone && !isMulti && symbol.last == ']'
    def ownerChain: List[String] = {
      val buf = List.newBuilder[String]
      def loop(symbol: String): Unit = {
        if (!symbol.isNone) {
          loop(symbol.owner)
          buf += symbol
        }
      }
      loop(symbol)
      buf.result
    }
    def owner: String = {
      if (isGlobal) {
        if (isRootPackage) Symbols.None
        else {
          val rest = DescriptorParser(symbol)._2
          if (rest.nonEmpty) rest
          else Symbols.RootPackage
        }
      } else {
        Symbols.None
      }
    }
    def desc: Descriptor = {
      if (isGlobal) {
        DescriptorParser(symbol)._1
      } else {
        d.None
      }
    }
  }

  sealed trait Descriptor {
    def isNone: Boolean = this == d.None
    def isTerm: Boolean = this.isInstanceOf[d.Term]
    def isMethod: Boolean = this.isInstanceOf[d.Method]
    def isType: Boolean = this.isInstanceOf[d.Type]
    def isPackage: Boolean = this.isInstanceOf[d.Package]
    def isParameter: Boolean = this.isInstanceOf[d.Parameter]
    def isTypeParameter: Boolean = this.isInstanceOf[d.TypeParameter]
    def value: String
    def name: n.Name = {
      this match {
        case d.None => n.TermName(value)
        case d.Term(value) => n.TermName(value)
        case d.Method(value, disambiguator) => n.TermName(value)
        case d.Type(value) => n.TypeName(value)
        case d.Package(value) => n.TermName(value)
        case d.Parameter(value) => n.TermName(value)
        case d.TypeParameter(value) => n.TypeName(value)
      }
    }
    override def toString: String = {
      this match {
        case d.None => sys.error("unsupported descriptor")
        case d.Term(value) => s"${n.encode(value)}."
        case d.Method(value, disambiguator) => s"${n.encode(value)}${disambiguator}."
        case d.Type(value) => s"${n.encode(value)}#"
        case d.Package(value) => s"${n.encode(value)}/"
        case d.Parameter(value) => s"(${n.encode(value)})"
        case d.TypeParameter(value) => s"[${n.encode(value)}]"
      }
    }
  }
  object Descriptor {
    case object None extends Descriptor { def value: String = "" }
    final case class Term(value: String) extends Descriptor
    final case class Method(value: String, disambiguator: String) extends Descriptor
    final case class Type(value: String) extends Descriptor
    final case class Package(value: String) extends Descriptor
    final case class Parameter(value: String) extends Descriptor
    final case class TypeParameter(value: String) extends Descriptor
  }

  object Names {
    // NOTE: This trait is defined inside Names to support the idiom of importing
    // scala.meta.internal.semanticdb.Scala._ and not being afraid of name conflicts.
    sealed trait Name {
      def value: String
      override def toString: String = value
    }
    final case class TermName(value: String) extends Name
    final case class TypeName(value: String) extends Name

    val RootPackage: TermName = TermName("_root_")
    val EmptyPackage: TermName = TermName("_empty_")
    val PackageObject: TermName = TermName("package")
    val Constructor: TermName = TermName("<init>")

    private[semanticdb] def encode(value: String): String = {
      if (value == "scalaShadowing") {
        "scala"
      } else if (value == "") {
        "``"
      } else {
        val (start, parts) = (value.head, value.tail)
        val isStartOk = Character.isJavaIdentifierStart(start)
        val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
        if (isStartOk && isPartsOk) value
        else "`" + value + "`"
      }
    }
  }

  object DisplayNames {
    val RootPackage: String = "_root_"
    val EmptyPackage: String = "_empty_"
    val Constructor: String = "<init>"
    val Anonymous: String = "_"
  }

  private class DescriptorParser(s: String) {
    var i = s.length
    def fail() = {
      val message = "invalid symbol format"
      val caret = " " * i + "^"
      sys.error(s"$message$EOL$s$EOL$caret")
    }

    val BOF = '\u0000'
    val EOF = '\u001A'
    var currChar = EOF
    def readChar(): Char = {
      if (i <= 0) {
        if (i == 0) {
          i -= 1
          currChar = BOF
          currChar
        } else {
          fail()
        }
      } else {
        i -= 1
        currChar = s(i)
        currChar
      }
    }

    def parseValue(): String = {
      if (currChar == '`') {
        val end = i
        while (readChar() != '`') {}
        readChar()
        s.substring(i + 2, end)
      } else {
        val end = i + 1
        if (!Character.isJavaIdentifierPart(currChar)) fail()
        while (Character.isJavaIdentifierPart(readChar()) && currChar != BOF) {}
        s.substring(i + 1, end)
      }
    }

    def parseDisambiguator(): String = {
      val end = i + 1
      if (currChar != ')') fail()
      while (readChar() != '(') {}
      readChar()
      s.substring(i + 1, end)
    }

    def parseDescriptor(): Descriptor = {
      if (currChar == '.') {
        readChar()
        if (currChar == ')') {
          val disambiguator = parseDisambiguator()
          val value = parseValue()
          d.Method(value, disambiguator)
        } else {
          d.Term(parseValue())
        }
      } else if (currChar == '#') {
        readChar()
        d.Type(parseValue())
      } else if (currChar == '/') {
        readChar()
        d.Package(parseValue())
      } else if (currChar == ')') {
        readChar()
        val value = parseValue()
        if (currChar != '(') fail()
        else readChar()
        d.Parameter(value)
      } else if (currChar == ']') {
        readChar()
        val value = parseValue()
        if (currChar != '[') fail()
        else readChar()
        d.TypeParameter(value)
      } else {
        fail()
      }
    }

    def entryPoint(): (Descriptor, String) = {
      readChar()
      val desc = parseDescriptor()
      (desc, s.substring(0, i + 1))
    }
  }

  private[semanticdb] object DescriptorParser {
    def apply(symbol: String): (Descriptor, String) = {
      val parser = new DescriptorParser(symbol)
      parser.entryPoint()
    }
  }
}
