package json.value.spec
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonReader, JsonWriter, ReaderConfig, readFromArray, readFromString}
import json.value.spec.codec.{JsArrayCodec, JsObjCodec}
import json.value.spec.*
import json.value.*
import json.value.spec.parser.*
import scala.collection.immutable
import java.time.Instant
import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.SeqMap
sealed trait JsSpec:
  def nullable: JsSpec = this.or(IsNull)

  def parser:Parser[_]

  def or(other: JsSpec): JsSpec = new JsSpec :
    override def validate(value: JsValue): Result =
      JsSpec.this.validate(value) match
        case Valid => Valid
        case _ => other.validate(value)
    override def parser =  JsSpec.this.parser.or(other.parser)


  def validate(value: JsValue): Result

private[value] sealed trait SchemaSpec[T<:Json[T]] extends JsSpec :
  def validateAll(json: T): LazyList[(JsPath, Invalid)]

sealed trait JsObjSchema extends SchemaSpec[JsObj]:
  override def validate(value: JsValue) =
    value match
      case o:JsObj => validateAll(o).headOption match
        case Some((_, error)) => error
        case None => Valid
      case _ => Invalid(value,SpecError.OBJ_EXPECTED)

  override def parser: JsonParser[JsObj]

sealed case class IsMapOfInt(valueSuchThat:Int=>Boolean|String= _=>true,
                             keySuchThat:String=>Boolean|String= _=>true) extends JsObjSchema:
  override def validateAll(json: JsObj): LazyList[(JsPath, Invalid)] =
    validateAllMapOfInt(JsPath.root,json,keySuchThat,valueSuchThat)

  override def parser:MapParser =
    MapParser(JsIntParser,toJsIntPredicate(valueSuchThat),keySuchThat)

object IsMapOfInt extends IsMapOfInt(_=>true, _=>true)

sealed case class IsMapOfLong(valueSuchThat:Long => Boolean|String = _=>true,
                              keySuchThat:String=>Boolean|String= _=>true) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfLong(JsPath.root,json,keySuchThat,valueSuchThat)

  override def parser:MapParser =
    MapParser(JsLongParser,toJsLongPredicate(valueSuchThat),keySuchThat)

object IsMapOfLong extends IsMapOfLong(_=>true, _=>true)

sealed case class IsMapOfInstant(valueSuchThat:Instant=>Boolean|String = _=>true,
                                 keySuchThat:String=>Boolean|String = _=>true) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfInstant(JsPath.root,json,keySuchThat,valueSuchThat)

  override def parser:MapParser =
    MapParser(JsInstantParser,toJsInstantPredicate(valueSuchThat),keySuchThat)

object IsMapOfInstant extends IsMapOfInstant(_=>true, _=>true)

sealed case class IsMapOfBool(keySuchThat:String=>Boolean= _=>true) extends JsObjSchema:
  override def validateAll(json: JsObj) = validateAllMapOfBool(JsPath.root,json,keySuchThat)

  override def parser:MapParser = MapParser(JsBoolParser,_=>true,keySuchThat)

object IsMapOfBool extends IsMapOfBool(_=>true)

sealed case class IsMapOfNumber(valueSuchThat: BigDecimal =>Boolean|String= _=>true,
                                keySuchThat:String=>Boolean|String= _=>true,
                                decimalConf: DecimalConf=DecimalConf) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfBigDec(JsPath.root,json,keySuchThat,valueSuchThat)
  override def parser:MapParser =
    MapParser(JsDecimalParser(decimalConf),
              toJsBigDecPredicate(valueSuchThat),keySuchThat)

object IsMapOfNumber extends IsMapOfNumber(_=>true, _=>true,DecimalConf)

sealed case class IsMapOfIntegral(valueSuchThat: BigInt =>Boolean|String= _=>true,
                                  keySuchThat:String=>Boolean|String= _=>true,
                                  digitsLimit:Int = BigIntConf.DIGITS_LIMIT) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfBigInt(JsPath.root,json,keySuchThat,valueSuchThat)
  override def parser:MapParser =
    MapParser(JsBigIntParser(digitsLimit),toJsBigIntPredicate(valueSuchThat),keySuchThat)

object IsMapOfIntegral extends IsMapOfIntegral(_=>true, _=>true,BigIntConf.DIGITS_LIMIT)

sealed case class IsMapOfStr(valueSuchThat:String=>Boolean|String= _=>true,
                             keySuchThat:String=>Boolean|String= _=>true) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfString(JsPath.root,json,keySuchThat,valueSuchThat)
  override def parser:MapParser =
    MapParser(JsStrParser,toJsStrPredicate(valueSuchThat),keySuchThat)

object IsMapOfStr extends IsMapOfStr(_=>true, _=>true)

sealed case class IsMapOfObj(valueSuchThat:JsObj=>Boolean|String= _=>true,
                             keySuchThat:String=>Boolean|String= _=>true,
                             decimalConf: DecimalConf=DecimalConf,
                             digitsLimit:Int = BigIntConf.DIGITS_LIMIT) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfJsObj(JsPath.root,json,keySuchThat,valueSuchThat)
  override def parser:MapParser =
    MapParser(JsObjParser(decimalConf,digitsLimit),toJsObjPredicate(valueSuchThat),keySuchThat)

object IsMapOfObj extends IsMapOfObj(_=>true, _=>true,DecimalConf,BigIntConf.DIGITS_LIMIT)

sealed case class IsMapOfArr(valueSuchThat:JsArray=>Boolean|String= _=>true,
                             keySuchThat:String=>Boolean|String= _=>true,
                             decimalConf: DecimalConf=DecimalConf,
                             bigIntDigitsLimit:Int=BigIntConf.DIGITS_LIMIT) extends JsObjSchema:
  override def validateAll(json: JsObj) =
    validateAllMapOfArr(JsPath.root,json,keySuchThat,valueSuchThat)

  override def parser:MapParser =
    MapParser(JsArrayOfParser(JsValueParser(decimalConf,bigIntDigitsLimit)),toJsArrayPredicate(valueSuchThat),keySuchThat)

object IsMapOfArr extends IsMapOfArr(_=>true, _=>true,DecimalConf,BigIntConf.DIGITS_LIMIT)

sealed case class JsObjSpec(private[spec] val specs: Map[String, JsSpec],
                            private[spec] val strict: Boolean = true,
                            private[spec] val required: Seq[String]) extends JsObjSchema :

  override def validateAll(json: JsObj): LazyList[(JsPath, Invalid)] =
    validateObjAll(JsPath.root, json, specs, strict, required)

  override def parser:JsObjSpecParser =
    JsObjSpecParser(specs.map((key,spec) => (key,spec.parser)),strict,required,null)

  def or(other:JsObjSchema) :JsObjSchema = new JsObjSchema:
    override def validateAll(json: JsObj): LazyList[(JsPath, Invalid)] =
      val errors = JsObjSpec.this.validateAll(json)
      if errors.isEmpty then LazyList.empty
      else other.validateAll(json)

    override def parser:JsonParser[JsObj] = new JsonParser[JsObj]:
      override def parse(json: String): JsObj =
        try JsObjSpec.this.parser.parse(json,ParserConf.DEFAULT_READER_CONFIG)
        catch case _ => other.parser.parse(json,ParserConf.DEFAULT_READER_CONFIG)

      override def parse(json: Array[Byte]): JsObj =
        try JsObjSpec.this.parser.parse(json,ParserConf.DEFAULT_READER_CONFIG)
        catch case _ => other.parser.parse(json,ParserConf.DEFAULT_READER_CONFIG)

      override def parse(json: String,
                         config: ReaderConfig): JsObj =
        try JsObjSpec.this.parser.parse(json,config)
        catch case _ => other.parser.parse(json,config)

      override def parse(json: Array[Byte],
                         config: ReaderConfig): JsObj =
        try JsObjSpec.this.parser.parse(json,config)
        catch case _ => other.parser.parse(json,config)

      override private[json] def parse(reader: JsonReader) =
        reader.setMark()
        try JsObjSpec.this.parser.parse(reader)
        catch
          case _ =>
            reader.rollbackToMark()
            other.parser.parse(reader)

  def withRequiredKeys(keys:String*):JsObjSpec =
    for (key <- keys) {
      if !specs.contains(key)
      then throw IllegalArgumentException("required key '" + key + "'  not defined in spec")
    }
    JsObjSpec(specs,strict,keys)
  def withOptKeys(keys:String*):JsObjSpec =
    for (key <- keys) {
      if !specs.contains(key)
      then throw IllegalArgumentException("optional key '" + key + "'  not defined in spec")
    }
    JsObjSpec(specs,strict,specs.keys.toSeq.filter(!keys.contains(_)))
  

  def concat(other:JsObjSpec):JsObjSpec =
    JsObjSpec(specs ++ other.specs,strict, required ++ other.required);

  def updated(key:String, spec:JsSpec) =
    JsObjSpec(specs.updated(key,spec), strict, required)

  def lenient = new JsObjSpec(specs,false,required):
    override def parser: JsObjSpecParser =
      JsObjSpecParser(specs.map((key,spec) => (key,spec.parser)),
                      strict,
                      required,
                      JsValueParser.DEFAULT)


  //si es lenient pueden venir numeros decimales y se pueded cambiar la forma por defecto de leerlos
  def lenient(decimalConf: DecimalConf,
              bigIntDigitsLimit:Int) = new JsObjSpec(specs, false, required):
    override def parser: JsObjSpecParser =
      JsObjSpecParser(specs.map((key, spec) => (key, spec.parser)),
                      strict,
                      required,
                      JsValueParser(decimalConf,bigIntDigitsLimit))

object JsObjSpec:
  def apply(pairs:(String,JsSpec)*):JsObjSpec =
    def toMap(pairs:List[(String,JsSpec)],
              result: immutable.SeqMap[String,JsSpec]): SeqMap[String,JsSpec] =
      if pairs.isEmpty then result
      else
        val head = pairs.head
        toMap(pairs.tail,result.updated(head._1,head._2))
    new JsObjSpec(toMap(pairs.toList,immutable.SeqMap.empty),true,pairs.map(_._1))

final case class IsTuple(specs: Seq[JsSpec], strict: Boolean = true) extends SchemaSpec[JsArray] :
  override def validateAll(json: JsArray) =
    validateArrAll(JsPath.root / 0, json, specs, strict)
  override def parser:JsArraySpecParser =
    JsArraySpecParser(specs.map(_.parser),strict)
  override def validate(value: JsValue) =
    value match
      case a:JsArray => validateAll(a).headOption match
        case Some((_, error)) => error
        case None => Valid
      case _ => Invalid(value,SpecError.ARRAY_EXPECTED)
  def appendedAll(other:IsTuple) = new IsTuple(specs.appendedAll(other.specs),strict)    

object IsTuple:
  def apply(spec: JsSpec*) = new IsTuple(spec,true)

private sealed trait JsValueSpec extends JsSpec :
  def validate(value: JsValue): Result

final case class IsArrayOf(spec: JsSpec) extends SchemaSpec[JsArray] :

  override def validateAll(json: JsArray): LazyList[(JsPath, Invalid)] =
    validateArrAll(JsPath.root / 0,json, json.seq.map(_=>spec),true)
  override def validate(value: JsValue): Result = value match
    case JsArray(seq) =>
      seq.map(it => spec.validate(it)).find(_ match
        case Valid => false
        case _ => true) match
        case Some(error) => error
        case None => Valid
    case _ => Invalid(value,SpecError.ARRAY_EXPECTED)

  override def parser:JsArrayOfParser =
    JsArrayOfParser(spec.parser)


object IsAny extends IsAny(_=>true,DecimalConf,BigIntConf.DIGITS_LIMIT)


sealed case class IsAny(suchThat:JsValue => Boolean|String,
                        decimalConf: DecimalConf=DecimalConf,
                        bigIntDigitsLimit:Int=BigIntConf.DIGITS_LIMIT) extends JsValueSpec:
  override def validate(value: JsValue): Result =
    suchThat(value) match
      case x:Boolean =>
        if x then Valid else Invalid(value,SpecError.VALUE_CONDITION_FAILED)
      case x:String => Invalid(value,SpecError(x))


  override def parser =
    JsValueParser(decimalConf,bigIntDigitsLimit).suchThat(suchThat)


object IsNull extends JsValueSpec :
  override def validate(value: JsValue): Result = value match
    case JsNull => Valid
    case _ => Invalid(value,SpecError.NULL_EXPECTED)
  override def parser = JsNullParser

sealed case class IsInt(suchThat:Int=>Boolean | String) extends JsValueSpec :

  override def validate(value: JsValue): Result = value match
    case x:JsInt => suchThat(x.value)  match
      case x:Boolean =>
        if x then Valid else Invalid(value,SpecError.INT_CONDITION_FAILED)
      case x:String => Invalid(value,SpecError(x))
    case _ => Invalid(value,SpecError.INT_EXPECTED)
  override def parser =
    JsIntParser.suchThat(toJsIntPredicate(suchThat))

object IsInt extends IsInt(_=>true)


sealed case class IsBool() extends JsValueSpec :
  override def validate(value: JsValue): Result = value match
    case JsBool(_) => Valid
    case _ => Invalid(value,SpecError.BOOLEAN_EXPECTED)
  override def parser = JsBoolParser

object IsBool extends IsBool
sealed case class IsLong(suchTaht:Long=>Boolean|String) extends JsValueSpec :

  override def validate(value: JsValue): Result =
    def validateLong(n: Long) =
      suchTaht(n) match
        case x: Boolean =>
          if x then Valid else Invalid(value, SpecError.LONG_CONDITION_FAILED)
        case x: String => Invalid(value, SpecError(x))
    value match
      case JsInt(n) => validateLong(n)
      case JsLong(n) => validateLong(n)
      case _ => Invalid(value,SpecError.LONG_EXPECTED)
  override def parser =
    JsLongParser.suchThat(toJsLongPredicate(suchTaht))

object IsLong extends IsLong(_=>true)

sealed case class IsStr(suchThat:String=>Boolean|String) extends JsValueSpec :
  override def validate(value: JsValue): Result = value match
    case JsStr(x) => suchThat(x) match
      case x:Boolean => if x then Valid else Invalid(value,SpecError.STRING_CONDITION_FAILED)
      case x:String => Invalid(value,SpecError(x))
    case _ => Invalid(value,SpecError.STRING_EXPECTED)
  override def parser = JsStrParser.suchThat(toJsStrPredicate(suchThat))

object IsStr extends IsStr(_=>true)

def IsCons[T<:JsValue](cons:T):JsSpec =
  IsAny(value =>
    if value == cons
    then true
    else SpecError.CONS_EXPECTED.message)

def IsEnum[T<:JsValue](cons:T*):JsSpec =
  IsAny(value =>
    if cons.contains(value)
    then true
    else SpecError.ENUM_VAL_EXPECTED.message)

sealed case class IsInstant(suchThat:Instant=>Boolean|String) extends JsValueSpec :

  override def validate(value: JsValue): Result =

    def validateInstant(x: Instant) =
      suchThat(x) match
        case x: Boolean => if x then Valid else Invalid(value, SpecError.INSTANT_CONDITION_FAILED)
        case x: String => Invalid(value, SpecError(x))
    value match
      case JsInstant(x) => validateInstant(x)
      case JsStr(x) =>
         JsStr.instantPrism.getOption(x) match
           case Some(i) => validateInstant(i)
           case None =>   Invalid(value,SpecError.INSTANT_EXPECTED)
      case _ => Invalid(value,SpecError.INSTANT_EXPECTED)
  override def parser =
    JsInstantParser.suchThat(toJsInstantPredicate(suchThat))

object IsInstant extends IsInstant(_=>true)

sealed case class IsNumber(suchThat:BigDecimal=>Boolean|String,
                           decimalConf: DecimalConf=DecimalConf) extends JsValueSpec :
  override def validate(value: JsValue): Result =
    def validateDec(dec:BigDecimal)=
      suchThat(dec) match
        case x:Boolean => if x then Valid else Invalid(value,SpecError.DECIMAL_CONDITION_FAILED)
        case x:String => Invalid(value,SpecError(x))
    value match
      case JsInt(n) => validateDec(n)
      case JsLong(n) => validateDec(n)
      case JsDouble(n) => validateDec(n)
      case JsBigDec(n) => validateDec(n)
      case JsBigInt(n) => validateDec(BigDecimal(n))
      case _ => Invalid(value,SpecError.DECIMAL_EXPECTED)
  override def parser =
    JsDecimalParser(decimalConf).suchThat(toJsBigDecPredicate(suchThat))

object IsNumber extends IsNumber(_=>true,DecimalConf)

sealed case class IsIntegral(suchThat:BigInt=>Boolean|String,
                             digitsLimit:Int=BigIntConf.DIGITS_LIMIT) extends JsValueSpec :

  override def validate(value: JsValue): Result =
    def validateBigInt(b: BigInt) =
      suchThat(b) match
        case x: Boolean => if x then Valid else Invalid(value, SpecError.BIG_INTEGER_CONDITION_FAILED)
        case x: String => Invalid(value, SpecError(x))
    value match
        case JsInt(n) => validateBigInt(n)
        case JsLong(n) => validateBigInt(n)
        case JsBigInt(n) => validateBigInt(n)
        case _ => Invalid(value,SpecError.BIG_INTEGER_EXPECTED)
  override def parser =
    JsBigIntParser(digitsLimit).suchThat(toJsBigIntPredicate(suchThat))

object IsIntegral extends IsIntegral(_=>true,BigIntConf.DIGITS_LIMIT)


sealed case class IsJsObj(suchThat:JsObj=>Boolean|String,
                          decimalConf: DecimalConf=DecimalConf,
                          digitsLimit: Int = BigIntConf.DIGITS_LIMIT) extends JsValueSpec :
  override def validate(value: JsValue): Result = value match
    case n:JsObj =>
      suchThat(n) match
        case x:Boolean => if x then Valid else Invalid(value,SpecError.OBJ_CONDITION_FAILED)
        case x:String => Invalid(value,SpecError(x))
    case _ => Invalid(value,SpecError.OBJ_EXPECTED)
  override def parser =
    JsObjParser(decimalConf,digitsLimit).suchThat(toJsObjPredicate(suchThat))

object IsJsObj extends IsJsObj(_=>true,DecimalConf,BigIntConf.DIGITS_LIMIT)


sealed case class IsArray(suchThat:JsArray => Boolean|String,
                          decimalConf: DecimalConf=DecimalConf,
                          bigIntDigitsLimit:Int=BigIntConf.DIGITS_LIMIT) extends JsValueSpec :
  override def validate(value: JsValue): Result = value match
    case n:JsArray => suchThat(n)  match
      case x:Boolean => if x then Valid else Invalid(value,SpecError.ARRAY_CONDITION_FAILED)
      case x:String => Invalid(value,SpecError(x))
    case _ => Invalid(value,SpecError.ARRAY_EXPECTED)
  override def parser =
    JsArrayOfParser(JsValueParser(decimalConf,bigIntDigitsLimit)).suchThat(toJsArrayPredicate(suchThat))

object IsArray extends IsArray(_=>true,DecimalConf,BigIntConf.DIGITS_LIMIT)


private def validateObjAll(path: JsPath,
                           json: JsObj,
                           specs: Map[String, JsSpec],
                           strict: Boolean,
                           required: Seq[String]): LazyList[(JsPath, Invalid)] =
  def validateStrict(path: JsPath,keys:Set[String]):LazyList[(JsPath, Invalid)] =
    if !strict || keys.isEmpty
    then LazyList.empty
    else (path / keys.head,Invalid(JsNothing,SpecError.SPEC_FOR_VALUE_NOT_DEFINED)) #:: validateStrict(path,keys.tail)
  def validateObj(x: JsObj, path: JsPath,remaining:Map[String, JsSpec]): LazyList[(JsPath, Invalid)] =
    if remaining.isEmpty then return LazyList.empty
    val (key, spec) = remaining.head
    val value = x(key)
    value match
      case JsNothing =>
        if required.contains(key)
        then (path / key,Invalid(JsNothing,SpecError.KEY_REQUIRED)) #:: validateObj(x,path,remaining.tail)
        else  validateObj(x,path,remaining.tail)
      case _:JsValue =>
        spec match
            case JsObjSpec(ys, zs, r) => value match
              case o:JsObj =>  validateObjAll(path / key, o, ys, zs, r) #::: validateObj(x, path,remaining.tail)
              case _ => (path / key,Invalid(value,SpecError.OBJ_EXPECTED)) #:: validateObj(x, path,remaining.tail)
            case IsArrayOf(spec) => value match
              case a: JsArray => validateArrAll(path / key / 0,a, a.seq.map(_=>spec),true) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.ARRAY_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsTuple(ys, zs) => value match
              case a:JsArray =>  validateArrAll(path / key / 0, a, ys, zs)  #:::  validateObj(x, path,remaining.tail)
              case _ => (path / key,Invalid(value,SpecError.ARRAY_EXPECTED)) #:: validateObj(x, path,remaining.tail)
            case IsMapOfInt(p,k) => value match
              case o:JsObj => validateAllMapOfInt(path / key,o,k,p) #:::  validateObj(x, path,remaining.tail)
              case _ => (path / key,Invalid(value,SpecError.OBJ_EXPECTED)) #:: validateObj(x, path,remaining.tail)
            case IsMapOfStr(p, k) => value match
              case o: JsObj => validateAllMapOfString(path / key, o, k, p) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfLong(p, k) => value match
              case o: JsObj => validateAllMapOfLong(path / key, o, k, p) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfIntegral(p, k,_) => value match
              case o: JsObj => validateAllMapOfBigInt(path / key, o, k, p) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfNumber(p, k,_) => value match
              case o: JsObj =>  validateAllMapOfBigDec(path / key, o, k, p) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfInstant(p, k) => value match
              case o: JsObj => validateAllMapOfInstant(path / key, o, k, p) #:::  validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfBool(k) => value match
              case o: JsObj => validateAllMapOfBool(path / key, o, k) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfArr(p, k,_,_) => value match
              case o: JsObj => validateAllMapOfArr(path / key, o, k, p)  #:::  validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case IsMapOfObj(p, k,_,_) => value match
              case o: JsObj => validateAllMapOfJsObj(path / key, o, k, p) #::: validateObj(x, path, remaining.tail)
              case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateObj(x, path, remaining.tail)
            case valueSpec: JsSpec => valueSpec.validate(value) match
              case Valid => validateObj(x, path,remaining.tail)
              case error: Invalid => (path / key, error) #:: validateObj(x, path,remaining.tail)

  val errors = validateObj(json, path,specs)
  if required.isEmpty then errors
  else errors #::: validateStrict(path,json.keys.toSet -- specs.keys)

private def validateArrAll(path: JsPath,
                           json: JsArray,
                           specs: Seq[JsSpec],
                           strict: Boolean): LazyList[(JsPath, Invalid)] =
  def validateArr(x: JsArray, y: Seq[JsSpec], path: JsPath): LazyList[(JsPath, Invalid)] =
    if x.isEmpty then return LazyList.empty
    if y.isEmpty && strict
    then return (path, Invalid(x.head,SpecError.SPEC_FOR_VALUE_NOT_DEFINED)) #:: LazyList.empty
    val value = x.head
    y.head match
      case JsObjSpec(z, s, r) => value match
        case o:JsObj => validateObjAll(path, o, z, s, r) #::: validateArr(x.tail, y.tail, path.inc)
        case _ => (path,Invalid(value,SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsTuple(z, s) => value match
        case a: JsArray => validateArrAll(path / 0, a, z, s) #::: validateArr(x.tail, y.tail, path.inc)
        case _ => (path, Invalid(value, SpecError.ARRAY_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsArrayOf(spec) => value match
        case a: JsArray => validateArrAll(path / 0, a, a.seq.map(_ => spec), true) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.ARRAY_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfInt(p, k) => value match
        case o: JsObj => validateAllMapOfInt(path , o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfStr(p, k) => value match
        case o: JsObj => validateAllMapOfString(path , o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfLong(p, k) => value match
        case o: JsObj => validateAllMapOfLong(path , o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfIntegral(p, k, _) => value match
        case o: JsObj => validateAllMapOfBigInt(path , o, k, p) #::: validateArr(x.tail, y.tail, path.inc)
        case _ => (path, Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfNumber(p, k, _) => value match
        case o: JsObj => validateAllMapOfBigDec(path, o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfInstant(p, k) => value match
        case o: JsObj => validateAllMapOfInstant(path , o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfBool(k) => value match
        case o: JsObj => validateAllMapOfBool(path , o, k) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfArr(p, k, _, _) => value match
        case o: JsObj => validateAllMapOfArr(path , o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case IsMapOfObj(p, k, _, _) => value match
        case o: JsObj => validateAllMapOfJsObj(path, o, k, p) #:::  validateArr(x.tail, y.tail, path.inc)
        case _ => (path , Invalid(value, SpecError.OBJ_EXPECTED)) #:: validateArr(x.tail, y.tail, path.inc)
      case valueSpec: JsSpec => valueSpec.validate(value) match
        case Valid => validateArr(x.tail, y.tail, path.inc)
        case error: Invalid => (path, error) #:: validateArr(x.tail, y.tail, path.inc)
  validateArr(json, specs, path)


private[spec] def toJsIntPredicate(p:Int=>Boolean|String):JsValue=>Boolean|String =
  x =>
    x match
      case JsInt(n) => p(n)
      case _ => false


private[spec] def toJsLongPredicate(p:Long=>Boolean|String):JsValue=>Boolean|String =
 x =>
   x match
      case JsInt(n) => p(n)
      case JsLong(n) => p(n)
      case _ => false

private[spec] def toJsBigIntPredicate(p:BigInt=>Boolean|String):JsValue=>Boolean|String  =
  x =>
    x match
      case JsInt(n) => p(n)
      case JsLong(n) => p(n)
      case JsBigInt(n) => p(n)
      case _ => false

private[spec] def toJsBigDecPredicate(p:BigDecimal=>Boolean|String):JsValue=>Boolean|String  =
  x =>
    x match
      case JsInt(n) => p(n)
      case JsLong(n) => p(n)
      case JsDouble(n) => p(n)
      case JsBigDec(n) => p(n)
      case JsBigInt(n) => p(BigDecimal(n))
      case _ => false

private[spec] def toJsObjPredicate(p:JsObj=>Boolean|String):JsValue=>Boolean|String  =
  x =>
    x match
      case n:JsObj => p(n)
      case _ => false

private[spec] def toJsArrayPredicate(p:JsArray=>Boolean|String):JsValue=>Boolean|String  =
   x =>
     x match
      case n:JsArray => p(n)
      case _ => false

private[spec] def toJsStrPredicate(p:String=>Boolean|String):JsValue=>Boolean|String  =
  x =>
    x match
      case JsStr(n) => p(n)
      case _ => false

private[spec] def toJsInstantPredicate(p:Instant=>Boolean|String):JsValue=>Boolean | String  =
  x =>
    x match
      case JsInstant(n) => p(n)
      case JsStr(n) => JsStr.instantPrism.getOption(n).map(p).getOrElse(false)
      case _ => false

private def validateAllMapOfInt(path: JsPath,
                                json: JsObj,
                                k:String=>Boolean|String,
                                p:Int=>Boolean|String): LazyList[(JsPath, Invalid)] =
  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head

  value match
    case JsInt(i) => p(i) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfInt(path,json.tail,k,p)
        else (path / key, Invalid(value, SpecError.INT_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfInt(path, json.tail,k,p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfInt(path, json.tail,k,p)
    case _ => (path / key, Invalid(value, SpecError.INT_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfInt(path, json.tail,k,p)



private def validateAllMapOfString(path: JsPath,
                                   json: JsObj,
                                   k:String=>Boolean|String,
                                   p:String=>Boolean|String): LazyList[(JsPath, Invalid)]  =
  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case JsStr(i) => p(i) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfString(path,json.tail,k,p)
        else (path / key, Invalid(value, SpecError.STRING_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfString(path,json.tail,k,p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfString(path,json.tail,k,p)
    case _ => (path / key, Invalid(value, SpecError.STRING_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfString(path,json.tail,k,p)


private def validateAllMapOfBigInt(path: JsPath,
                                   json: JsObj,
                                   k: String => Boolean | String,
                                   p: BigInt => Boolean | String): LazyList[(JsPath, Invalid)] =
  @inline def validateHead(i: BigInt, value: JsValue, key: String) =
    p(i) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfBigInt(path,json.tail,k,p)
        else (path / key, Invalid(value, SpecError.BIG_INTEGER_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfBigInt(path,json.tail,k,p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfBigInt(path,json.tail,k,p)

  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case JsInt(i) => validateHead(i, value, key)
    case JsLong(i) => validateHead(i, value, key)
    case JsBigInt(i) => validateHead(i, value, key)
    case _ => (path / key, Invalid(value, SpecError.BIG_INTEGER_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfBigInt(path,json.tail,k,p)



private def validateAllMapOfBigDec(path: JsPath,
                                   json: JsObj,
                                   k: String => Boolean | String,
                                   p: BigDecimal => Boolean | String): LazyList[(JsPath, Invalid)] =
  @inline def validateHead(i: BigDecimal,
                           value: JsValue,
                           key: String) =
    p(i) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfBigDec(path,json.tail,k,p)
        else (path / key, Invalid(value, SpecError.DECIMAL_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfBigDec(path,json.tail,k,p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfBigDec(path,json.tail,k,p)

  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case JsInt(i) => validateHead(i, value, key)
    case JsLong(i) => validateHead(i, value, key)
    case JsDouble(i) => validateHead(i, value, key)
    case JsBigDec(i) => validateHead(i, value, key)
    case _ => (path / key, Invalid(value, SpecError.DECIMAL_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfBigDec(path,json.tail,k,p)



private def validateAllMapOfLong(path: JsPath,
                                 json: JsObj,
                                 k: String => Boolean | String,
                                 p: Long => Boolean | String): LazyList[(JsPath, Invalid)] =
  @inline def validateHead(i: Long, value: JsValue, key: String):LazyList[(JsPath, Invalid)] =
    p(i) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfLong(path,json.tail,k,p)
        else (path / key, Invalid(value, SpecError.LONG_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfLong(path,json.tail,k,p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfLong(path,json.tail,k,p)

  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case JsInt(i) => validateHead(i, value, key)
    case JsLong(i) => validateHead(i, value, key)
    case _ => (path / key, Invalid(value, SpecError.LONG_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfLong(path,json.tail,k,p)




private def validateAllMapOfInstant(path: JsPath,
                                    json: JsObj,
                                    k:String=>Boolean|String,
                                    p:Instant=>Boolean|String): LazyList[(JsPath, Invalid)]  =
  @inline def validateHead(key: String, x:String|Boolean,value:JsValue): LazyList[(JsPath, Invalid)] =
    x match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfInstant(path, json.tail, k, p)
        else (path / key, Invalid(value, SpecError.INSTANT_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfInstant(path, json.tail, k, p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfInstant(path, json.tail, k, p)
  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case JsInstant(i) => validateHead(key,p(i),value)
    case JsStr(i) =>
       JsStr.instantPrism.getOption(i).map(p(_)) match
         case Some(x) => validateHead(key,x,value)
         case None => (path / key, Invalid(value, SpecError.INSTANT_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfInstant(path,json.tail,k,p)
    case _ => (path / key, Invalid(value, SpecError.INSTANT_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfInstant(path,json.tail,k,p)


private def validateAllMapOfJsObj(path: JsPath,
                                  json: JsObj,
                                  k:String=>Boolean|String,
                                  p:JsObj=>Boolean|String): LazyList[(JsPath, Invalid)]  =
  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case o: JsObj => p(o) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfJsObj(path,json.tail,k,p)
        else (path / key, Invalid(o, SpecError.OBJ_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfJsObj(path,json.tail,k,p)
      case x: String => (path / key, Invalid(o, SpecError(x))) #::  validateKey(key, path,k) #::: validateAllMapOfJsObj(path,json.tail,k,p)
    case _ => (path / key, Invalid(value, SpecError.OBJ_EXPECTED)) #::  validateKey(key, path,k) #::: validateAllMapOfJsObj(path,json.tail,k,p)



private def validateAllMapOfArr(path: JsPath,
                                json: JsObj,
                                k:String => Boolean|String,
                                p:JsArray => Boolean|String): LazyList[(JsPath, Invalid)] =
  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case o: JsArray => p(o) match
      case x: Boolean =>
        if x then validateKey(key, path,k) #::: validateAllMapOfArr(path,json.tail,k,p)
        else (path / key, Invalid(value, SpecError.ARRAY_CONDITION_FAILED)) #:: validateKey(key, path,k) #::: validateAllMapOfArr(path,json.tail,k,p)
      case x: String => (path / key, Invalid(value, SpecError(x))) #:: validateKey(key, path,k) #::: validateAllMapOfArr(path,json.tail,k,p)
    case _ => (path / key, Invalid(value, SpecError.ARRAY_EXPECTED)) #:: validateKey(key, path,k) #::: validateAllMapOfArr(path,json.tail,k,p)

private def validateAllMapOfBool(path: JsPath,
                                 json: JsObj,
                                 k: String => Boolean | String): LazyList[(JsPath, Invalid)] =
  if json.isEmpty then return LazyList.empty
  val (key, value) = json.head
  value match
    case JsBool(_) => validateKey(key, path, k) #::: validateAllMapOfBool(path, json.tail, k)
    case _ => (path / key, Invalid(value, SpecError.BOOLEAN_EXPECTED)) #:: validateKey(key, path, k) #:::  validateAllMapOfBool(path, json.tail, k)

private def validateKey(key: String,
                        path:JsPath,
                        k:String=>String|Boolean):LazyList[(JsPath, Invalid)] =
  k(key) match
    case x: Boolean =>
      if x then LazyList.empty
      else LazyList((path / key, Invalid(JsStr(key), SpecError.KEY_CONDITION_FAILED)))
    case x: String => LazyList((path / key, Invalid(JsStr(key), SpecError(x))))