package json.value.spec.parser
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonReaderException, ReaderConfig, readFromArray, readFromString}
import json.value.{JsArray, JsBigDec, JsBigInt, JsBool, JsDouble, JsInstant, JsInt, JsLong, JsNull, JsNumber, JsObj, JsStr, JsValue}

import java.math.MathContext
import scala.annotation.targetName



trait Parser[T <: JsValue]:

  private[json] def parse(reader: JsonReader):T


  def suchThat(predicate: T => Boolean|String): Parser[T] =
    (reader: JsonReader) =>
      val value = Parser.this.parse(reader)
      predicate(value) match
        case x:Boolean => if x then value
                          else throw reader.decodeError(ParserSpecError.SUCH_THAT_CONDITION_FAILED)
        case x:String => throw reader.decodeError(x)

  def or[Q <: JsValue](other: Parser[Q]): Parser[JsValue] =
    (reader: JsonReader) =>
      reader.setMark()
      try Parser.this.parse(reader)
      catch
        case _ =>
          reader.rollbackToMark()
          other.parse(reader)


private[json] final case class JsValueParser(decimalConf:DecimalConf,bigIntDigitsLimit:Int) extends Parser[JsValue] :
  override def parse(in: JsonReader) =
    val b = in.nextToken()
    if b == '"' then
      in.rollbackToken()
      return JsStrParser.parse(in)

    if b == 'f' || b == 't' then
      in.rollbackToken()
      return JsBoolParser.parse(in)

    if b >= '0' && b <= '9' || b == '-' then
      in.rollbackToken()
      return JsNumberParser(decimalConf).parse(in)

    if b == '[' then parseArrayAfterOpenSquareBracket(in,this)

    else if b == '{' then parseObjAfterOpenBrace(in,this)

    else in.readNullOrError(JsNull, ParserSpecError.INVALID_JSON_TOKEN)

private[json] object JsValueParser:
  val DEFAULT = new JsValueParser(DecimalConf,BigIntConf.DIGITS_LIMIT)

private[json] final case class JsNumberParser(decimalConf: DecimalConf) extends Parser[JsNumber] :
  override def parse(in: JsonReader) =
    in.setMark()
    var digits = 0
    var b = in.nextByte()
    if b == '-' then b = in.nextByte()
    try
      while b >= '0' && b <= '9' do
        b = in.nextByte()
        digits += 1
    catch
      case _: JsonReaderException => // ignore the end of input error for now
    finally in.rollbackToMark()

    if (b | 0x20) != 'e' && b != '.' then
      if digits < 19 then JsLongParser.parse(in)
      else
        val x = in.readBigInt(null)
        if x.bitLength < 64 then JsLong(x.longValue)
        else JsBigInt(x)
    else JsDecimalParser(decimalConf).parse(in)


private[json] object JsIntParser extends Parser[JsInt] :
  override def parse(reader: JsonReader) = JsInt(reader.readInt())

private[json] object JsLongParser extends Parser[JsLong]:
  override def parse(reader: JsonReader) = JsLong(reader.readLong())


private[json] final case class JsDecimalParser(decimalConf: DecimalConf = DecimalConf) extends Parser[JsBigDec] :
  override def parse(reader: JsonReader) =
    JsBigDec(reader.readBigDecimal(null,
      decimalConf.mathContext,
      decimalConf.scaleLimit,
      decimalConf.digitsLimit))

private[json] final case class JsBigIntParser(digitsLimit:Int) extends Parser[JsBigInt] :
  override def parse(reader: JsonReader) = JsBigInt(reader.readBigInt(null,digitsLimit))

private[json] object JsBigIntParser:
  val DEFAULT = new JsBigIntParser(BigIntConf.DIGITS_LIMIT)

private[json] object JsStrParser extends Parser[JsStr] :
  override def parse(reader: JsonReader) = JsStr(reader.readString(null))

private[json] object JsInstantParser extends Parser[JsInstant] :
  override def parse(reader: JsonReader) = JsInstant(reader.readInstant(null))

private[json] object JsBoolParser extends Parser[JsBool] :
  override def parse(reader: JsonReader) = JsBool(reader.readBoolean())

private[json] object JsNullParser extends Parser[JsNull.type] :
  override def parse(reader: JsonReader) =
    reader.nextToken()
    reader.readNullOrError(JsNull, ParserSpecError.NULL_EXPECTED)

private[json] final case class JsArrayOfParser(valueParser:Parser[_]) extends Parser[JsArray] :
  override def parse(in: JsonReader) =
    val b = in.nextToken()
    if b == '[' then parseArrayAfterOpenSquareBracket(in,valueParser)
    else in.decodeError(ParserSpecError.START_ARRAY_EXPECTED)

private[json] object JsArrayOfParser:
  val DEFAULT = JsArrayOfParser(JsValueParser.DEFAULT)

private[json] final case class JsObjParser(decimalConf: DecimalConf,bigIntDigitsLimit:Int) extends Parser[JsObj] :
  override def parse(in: JsonReader) =
    val b = in.nextToken()
    if b == '{' then parseObjAfterOpenBrace(in,JsValueParser(decimalConf,bigIntDigitsLimit))
    else in.decodeError(ParserSpecError.START_OBJECT_EXPECTED)

private[json] object JsObjParser:
  val DEFAULT = new JsObjParser(DecimalConf,BigIntConf.DIGITS_LIMIT)

private[parser] def parseArrayAfterOpenSquareBracket(in: JsonReader,
                                                     valueParser:Parser[_]): JsArray =
  if in.isNextToken(']') then return JsArray.empty
  in.rollbackToken()
  var x: List[JsValue] = List.empty
  var isNextToken = true
  while isNextToken do
    x = x.appended(valueParser.parse(in))
    isNextToken = in.isNextToken(',')
  if in.isCurrentToken(']') then JsArray(x)
  else in.arrayEndOrCommaError()

@inline
private[parser] def parseObjAfterOpenBrace(in: JsonReader,
                                           valueParser:Parser[_]): JsObj =
  parseObjAfterOpenBrace(in, valueParser,_=>true,_=>true)


private[parser] def parseObjAfterOpenBrace(in: JsonReader,
                                           valueParser:Parser[_],
                                           valuePredicate:JsValue => Boolean|String,
                                           keyPredicate:String=>Boolean|String): JsObj =
  if in.isNextToken('}') then JsObj.empty
  else
    in.rollbackToken()
    var map: Map[String, JsValue] = Map.empty
    var isNextToken = true
    while isNextToken do
      val key: String = in.readKeyAsString()
      keyPredicate(key) match
        case x: Boolean => if !x then in.decodeError(ParserSpecError.KEY_CONDITION_FAILED)
        case x: String => in.decodeError(x)
      val value = valueParser.parse(in)
      valuePredicate(value) match
        case x:Boolean =>
          if x then map = map.updated(key, value) else in.decodeError(ParserSpecError.VALUE_CONDITION_FAILED)
        case x:String => in.decodeError(x)
      isNextToken = in.isNextToken(',')
    if in.isCurrentToken('}') then new JsObj(map)
    else in.objectEndOrCommaError()