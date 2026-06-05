package dotty.tools.dotc.semanticdb

import scala.language.unsafeNulls

import java.lang.System.{lineSeparator => EOL}
import dotty.tools.dotc.semanticdb.{Descriptor => d}

private[semanticdb] class DescriptorParser(s: String) {
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

private[semanticdb] sealed trait Descriptor {
  def isNone: Boolean = this == d.None
  def isTerm: Boolean = this.isInstanceOf[d.Term]
  def isMethod: Boolean = this.isInstanceOf[d.Method]
  def isType: Boolean = this.isInstanceOf[d.Type]
  def isPackage: Boolean = this.isInstanceOf[d.Package]
  def isParameter: Boolean = this.isInstanceOf[d.Parameter]
  def isTypeParameter: Boolean = this.isInstanceOf[d.TypeParameter]
  def value: String
}

private[semanticdb] object Descriptor {
  case object None extends Descriptor { def value: String = "" }
  final case class Term(value: String) extends Descriptor
  final case class Method(value: String, disambiguator: String) extends Descriptor
  final case class Type(value: String) extends Descriptor
  final case class Package(value: String) extends Descriptor
  final case class Parameter(value: String) extends Descriptor
  final case class TypeParameter(value: String) extends Descriptor
}
