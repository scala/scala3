package dotty.internal

import scala.quoted._
import scala.quoted.matching._
import scala.tasty.Reflection
import reflect._

object StringContextMacro {

  /** Implementation of scala.StringContext.f used in Dotty */
  inline def f(sc: => StringContext)(args: Any*): String = ${ interpolate('sc, 'args) }

  /** This trait defines a tool to report errors/warnings that do not depend on Position. */
  trait Reporter {

    /** Reports error/warning of size 1 linked with a part of the StringContext.
      *
      * @param message the message to report as error/warning
      * @param index the index of the part inside the list of parts of the StringContext
      * @param offset the index in the part String where the error is
      * @return an error/warning depending on the function
      */
    def partError(message : String, index : Int, offset : Int) : Unit
    def partWarning(message : String, index : Int, offset : Int) : Unit

    /** Reports error linked with an argument to format.
      *
      * @param message the message to report as error/warning
      * @param index the index of the argument inside the list of arguments of the format function
      * @return an error depending on the function
      */
    def argError(message : String, index : Int) : Unit

    /** Reports error linked with the list of arguments or the StringContext.
      *
      * @param message the message to report in the error
      * @return an error
      */
    def strCtxError(message : String) : Unit
    def argsError(message : String) : Unit

    /** Claims whether an error or a warning has been reported
      *
      * @return true if an error/warning has been reported, false
      */
    def hasReported() : Boolean

    /** Stores the old value of the reported and reset it to false */
    def resetReported() : Unit

    /** Restores the value of the reported boolean that has been reset */
    def restoreReported() : Unit
  }

  /** Retrieves a String from an Expr containing it
   *
   *  @param expression the Expr containing the String
   *  @return the String contained in the given Expr
   *  quotes an error if the given Expr does not contain a String
   */
  private def literalToString(expression : Expr[String])(implicit reflect: Reflection) : String = expression match {
    case Const(string : String) => string
    case _ => QuoteError("Expected statically known literal", expression)
  }

  /** Retrieves the parts from a StringContext, given inside an Expr, and returns them as a list of Expr of String
   *
   *  @param strCtxExpr the Expr containing the StringContext
   *  @return a list of Expr containing Strings, each corresponding to one parts of the given StringContext
   *  quotes an error if the given Expr does not correspond to a StringContext
   */
  def getPartsExprs(strCtxExpr : Expr[scala.StringContext])(implicit reflect : Reflection): List[Expr[String]] = {
    import reflect._
    strCtxExpr match {
      case '{ StringContext(${ExprSeq(parts)}: _*) } => parts.toList
      case '{ new StringContext(${ExprSeq(parts)}: _*) } => parts.toList
      case _ => QuoteError("Expected statically known String Context", strCtxExpr)
    }
  }

  /** Retrieves a list of Expr, each containing an argument, from an Expr of list of arguments
   *
   *  @param argsExpr the Expr containing the list of arguments
   *  @return a list of Expr containing arguments
   *  quotes an error if the given Expr does not contain a list of arguments
   */
  def getArgsExprs(argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): List[Expr[Any]] = {
    import reflect._
    argsExpr.unseal.underlyingArgument match {
      case Typed(Repeated(args, _), _) => args.map(_.seal)
      case tree => QuoteError("Expected statically known argument list", argsExpr)
    }
  }

  /** Interpolates the arguments to the formatting String given inside a StringContext
   *
   *  @param strCtxExpr the Expr that holds the StringContext which contains all the chunks of the formatting string
   *  @param args the Expr that holds the sequence of arguments to interpolate to the String in the correct format
   *  @return the Expr containing the formatted and interpolated String or an error/warning if the parameters are not correct
   */
  private def interpolate(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]])(implicit reflect: Reflection): Expr[String] = {
    import reflect._
    val sourceFile = strCtxExpr.unseal.pos.sourceFile

    val partsExpr = getPartsExprs(strCtxExpr)
    val args = getArgsExprs(argsExpr)

    val reporter = new Reporter{
      private[this] var reported = false
      private[this] var oldReported = false
      def partError(message : String, index : Int, offset : Int) : Unit = {
        reported = true
        val positionStart = partsExpr(index).unseal.pos.start + offset
        reflect.error(message, sourceFile, positionStart, positionStart)
      }
      def partWarning(message : String, index : Int, offset : Int) : Unit = {
        reported = true
        val positionStart = partsExpr(index).unseal.pos.start + offset
        reflect.warning(message, sourceFile, positionStart, positionStart)
      }

      def argError(message : String, index : Int) : Unit = {
        reported = true
        reflect.error(message, args(index).unseal.pos)
      }

      def strCtxError(message : String) : Unit = {
        reported = true
        val positionStart = strCtxExpr.unseal.pos.start
        reflect.error(message, sourceFile, positionStart, positionStart)
      }
      def argsError(message : String) : Unit = {
        reported = true
        reflect.error(message, argsExpr.unseal.pos)
      }

      def hasReported() : Boolean = {
        reported
      }

      def resetReported() : Unit = {
        oldReported = reported
        reported = false
      }

      def restoreReported() : Unit = {
        reported = oldReported
      }
    }

    interpolate(partsExpr, args, argsExpr, reporter)
  }

  /** Helper function for the interpolate function above
   *
   *  @param partsExpr the list of parts enumerated as Expr
   *  @param args the list of arguments enumerated as Expr
   *  @param reporter the reporter to return any error/warning when a problem is encountered
   *  @return the Expr containing the formatted and interpolated String or an error/warning report if the parameters are not correct
   */
  def interpolate(partsExpr : List[Expr[String]], args : List[Expr[Any]], argsExpr: Expr[Seq[Any]], reporter : Reporter)(implicit reflect: Reflection) : Expr[String] = {
    import reflect._

    /** Checks if the number of arguments are the same as the number of formatting strings
     *
     *  @param format the number of formatting parts in the StringContext
     *  @param argument the number of arguments to interpolate in the string
     *  @return reports an error if the number of arguments does not match with the number of formatting strings,
     *  nothing otherwise
     */
    def checkSizes(format : Int, argument : Int) : Unit = {
      if (format > argument && !(format == -1 && argument == 0))
        if (argument == 0)
          reporter.argsError("too few arguments for interpolated string")
        else
          reporter.argError("too few arguments for interpolated string", argument - 1)
      if (format < argument && !(format == -1 && argument == 0))
        if (argument == 0)
          reporter.argsError("too many arguments for interpolated string")
        else
          reporter.argError("too many arguments for interpolated string", format)
      if (format == -1)
        reporter.strCtxError("there are no parts")
    }

    /** Adds the default "%s" to the Strings that do not have any given format
     *
     *  @param parts the list of parts contained in the StringContext
     *  @return a new list of string with all a defined formatting or reports an error if the '%' and
     *  formatting parameter are too far away from the argument that they refer to
     *  For example : f2"${d}random-leading-junk%d" will lead to an error
     */
    def addDefaultFormat(parts : List[String]) : List[String] = parts match {
      case Nil => Nil
      case p :: parts1 => p :: parts1.map((part : String) => {
        if (!part.startsWith("%")) {
          val index = part.indexOf('%')
          if (!reporter.hasReported() && index != -1) {
            reporter.partError("conversions must follow a splice; use %% for literal %, %n for newline", parts.indexOf(part), index)
            "%s" + part
          } else "%s" + part
        } else part
      })
    }

    /** Checks whether a part contains a formatting substring
     *
     *  @param part the part to check
     *  @param l the length of the given part
     *  @param index the index where to start to look for a potential new formatting string
     *  @return an Option containing the index in the part where a new formatting String starts, None otherwise
     */
    def getFormattingSubstring(part : String, l : Int, index : Int) : Option[Int] = {
      var i = index
      var result : Option[Int] = None
      while (i < l){
        if (part.charAt(i) == '%' && result.isEmpty)
          result = Some(i)
        i += 1
      }
      result
    }

    /** Finds all the flags that are inside a formatting String from a given index
     *
     *  @param i the index in the String s where to start to check
     *  @param l the length of s
     *  @param s the String to check
     *  @return a list containing all the flags that are inside the formatting String,
     *  and their index in the String
     */
    def getFlags(i : Int, l : Int, s : String) : List[(Char, Int)] = {
      def isFlag(c : Char) : Boolean = c match {
        case '-' | '#' | '+' | ' ' | '0' | ',' | '(' => true
        case _ => false
      }
      if (i < l && isFlag(s.charAt(i))) (s.charAt(i), i) :: getFlags(i + 1, l, s)
      else Nil
    }

    /** Skips the Characters that are width or argumentIndex parameters
     *
     *  @param i the index where to start checking in the given String
     *  @param s the String to check
     *  @param l the length of s
     *  @return a tuple containing the index in the String after skipping
     *  the parameters, true if it has a width parameter and its value, false otherwise
     */
    def skipWidth(i : Int, s : String, l : Int) = {
      var j = i
      var width = (false, 0)
      while (j < l && Character.isDigit(s.charAt(j))){
        width = (true, j)
        j += 1
      }
      (j, width._1, width._2)
    }

    /** Retrieves all the formatting parameters from a part and their index in it
     *
     *  @param part the String containing the formatting parameters
     *  @param argIndex the index of the current argument inside the list of arguments to interpolate
     *  @param partIndex the index of the current part inside the list of parts in the StringContext
     *  @param noArg true if there is no arg, i.e. "%%" or "%n"
     *  @param pos the initial index where to start checking the part
     *  @return reports an error if any of the size of the arguments and the parts do not match or if a conversion
     *  parameter is missing. Otherwise,
     *  the index where the format specifier substring is,
     *  hasArgumentIndex (true and the index of its corresponding argumentIndex if there is an argument index, false and 0 otherwise) and
     *  flags that contains the list of flags (empty if there is none),
     *  hasWidth (true and the index of the width parameter if there is a width, false and 0 otherwise),
     *  hasPrecision (true and the index of the precision if there is a precision, false and 0 otherwise),
     *  hasRelative (true if the specifiers use relative indexing, false otherwise) and
     *  conversion character index
     */
    def getFormatSpecifiers(part : String, argIndex : Int, partIndex : Int, noArg : Boolean, pos : Int) : (Boolean, Int, List[(Char, Int)], Boolean, Int, Boolean, Int, Boolean, Int, Int) = {
      var conversion = pos
      var hasArgumentIndex = false
      var argumentIndex = pos
      var hasPrecision = false
      var precision = pos
      val l = part.length

      if (l >= 1 && part.charAt(conversion) == '%')
        conversion += 1
      else if (!noArg)
        reporter.argError("too many arguments for interpolated string", argIndex)

      //argument index or width
      val (i, hasWidth1, width1) = skipWidth(conversion, part, l)
      conversion = i

      //argument index
      if (conversion < l && part.charAt(conversion) == '$'){
        if (hasWidth1){
          hasArgumentIndex = true
          argumentIndex = width1
          conversion += 1
        } else {
          reporter.partError("Missing conversion operator in '" + part.substring(0, conversion) + "'; use %% for literal %, %n for newline", partIndex, 0)
        }
      }

      //relative indexing
      val hasRelative = conversion < l && part.charAt(conversion) == '<'
      val relativeIndex = conversion
      if (hasRelative)
        conversion += 1

      //flags
      val flags = getFlags(conversion, l, part)
      conversion += flags.size

      //width
      val (j, hasWidth2, width2) = skipWidth(conversion, part, l)
      conversion = j

      //precision
      if (conversion < l && part.charAt(conversion) == '.') {
        precision = conversion
        conversion += 1
        hasPrecision = true
        val oldConversion = conversion
        while (conversion < l && Character.isDigit(part.charAt(conversion))) {
          conversion += 1
        }
        if (oldConversion == conversion) {
          reporter.partError("Missing conversion operator in '" + part.substring(pos, oldConversion - 1) + "'; use %% for literal %, %n for newline", partIndex, pos)
          hasPrecision = false
        }
      }

      //conversion
      if((conversion >= l || (!part.charAt(conversion).isLetter && part.charAt(conversion) != '%')) && !reporter.hasReported())
        reporter.partError("Missing conversion operator in '" + part.substring(pos, conversion) + "'; use %% for literal %, %n for newline", partIndex, pos)

      val hasWidth = (hasWidth1 && !hasArgumentIndex) || hasWidth2
      val width = if (hasWidth1 && !hasArgumentIndex) width1 else width2
      (hasArgumentIndex, argumentIndex, flags, hasWidth, width, hasPrecision, precision, hasRelative, relativeIndex, conversion)
    }

    /** Checks if a given type is a subtype of any of the possibilities
     *
     *  @param actualType the given type
     *  @param expectedType the type we are expecting
     *  @param argIndex the index of the argument that should type check
     *  @param possibilities all the types within which we want to find a super type of the actualType
     *  @return reports a type mismatch error if the actual type is not a subtype of any of the possibilities,
     *  nothing otherwise
     */
    def checkSubtype(actualType : Type, expectedType : String, argIndex : Int, possibilities : Type*) = {
      if (possibilities.find(actualType <:< _).isEmpty)
        reporter.argError("type mismatch;\n found   : " + actualType.widen.show.stripPrefix("scala.Predef.").stripPrefix("java.lang.").stripPrefix("scala.") + "\n required: " + expectedType, argIndex)
    }

    /** Checks whether a given argument index, relative or not, is in the correct bounds
     *
     *  @param partIndex the index of the part we are checking
     *  @param offset the index in the part where there might be an error
     *  @param relative true if relative indexing is used, false otherwise
     *  @param argumentIndex the argument index parameter in the formatting String
     *  @param expected true if we have an expectedArgumentIndex, false otherwise
     *  @param expectedArgumentIndex the expected argument index parameter
     *  @param maxArgumentIndex the maximum argument index parameter that can be used
     *  @return reports a warning if relative indexing is used but an argument is still given,
     *  an error is the argument index is not in the bounds [1, number of arguments]
     */
    def checkArgumentIndex(partIndex : Int, offset : Int, relative : Boolean, argumentIndex : Int, expected : Boolean, expectedArgumentIndex : Int, maxArgumentIndex : Int) = {
      if (relative)
        reporter.partWarning("Argument index ignored if '<' flag is present", partIndex, offset)

      if (argumentIndex > maxArgumentIndex || argumentIndex <= 0)
        reporter.partError("Argument index out of range", partIndex, offset)

      if (expected && expectedArgumentIndex != argumentIndex && !reporter.hasReported())
        reporter.partWarning("Index is not this arg", partIndex, offset)
    }

    /** Checks if a parameter is specified whereas it is not allowed
     *
     *  @param hasParameter true if parameter is specified, false otherwise
     *  @param partIndex the index of the part inside the parts
     *  @param offset the index in the part where to report an error
     *  @param parameter the parameter that is not allowed
     *  @return reports an error if hasParameter is true, nothing otherwise
     */
    def checkNotAllowedParameter(hasParameter : Boolean, partIndex : Int, offset : Int, parameter : String) = {
      if (hasParameter)
        reporter.partError(parameter + " not allowed", partIndex, offset)
    }

    /** Checks if the flags are allowed for the conversion
     *
     *  @param partIndex the index of the part in the String Context
     *  @param flags the specified flags to check
     *  @param notAllowedFlagsOnCondition a list that maps which flags are allowed depending on the conversion Char
     *  @return reports an error if the flag is not allowed, nothing otherwise
     */
    def checkFlags(partIndex : Int, flags : List[(Char, Int)], notAllowedFlagOnCondition : (Char, Boolean, String)*) = {
      for {flag <- flags ; (nonAllowedFlag, condition, message) <- notAllowedFlagOnCondition ; if (flag._1 == nonAllowedFlag && condition)}
        reporter.partError(message, partIndex, flag._2)
    }

    /** Checks if the flags are allowed for the conversion
     *
     *  @param partIndex the index of the part in the String Context
     *  @param flags the specified flags to check
     *  @param notAllowedFlagsOnCondition a list that maps which flags are allowed depending on the conversion Char
     *  @return reports an error only once if at least one of the flags is not allowed, nothing otherwise
     */
    def checkUniqueFlags(partIndex : Int, flags : List[(Char, Int)], notAllowedFlagOnCondition : (Char, Boolean, String)*) = {
      reporter.resetReported()
      for {flag <- flags ; (nonAllowedFlag, condition, message) <- notAllowedFlagOnCondition ; if (flag._1 == nonAllowedFlag && condition)} {
        if (!reporter.hasReported())
          reporter.partError(message, partIndex, flag._2)
      }
      if (!reporter.hasReported())
        reporter.restoreReported()
    }

    /** Checks all the formatting parameters for a Character conversion
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param flags the flags parameters inside the formatting part
     *  @param hasPrecision true if precision parameter is specified, false otherwise
     *  @param precision the index of the precision parameter inside the part
     *  @return reports an error
     *  if precision is specified or if the used flags are different from '-'
     */
    def checkCharacterConversion(partIndex : Int, flags : List[(Char, Int)], hasPrecision : Boolean, precisionIndex : Int) = {
      val notAllowedFlagOnCondition = for (flag <- List('#', '+', ' ', '0', ',', '(')) yield (flag, true, "Only '-' allowed for c conversion")
      checkUniqueFlags(partIndex, flags, notAllowedFlagOnCondition : _*)
      checkNotAllowedParameter(hasPrecision, partIndex, precisionIndex, "precision")
    }

    /** Checks all the formatting parameters for an Integral conversion
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param argType the type of the argument matching with the given part
     *  @param conversionChar the Char used for the formatting conversion
     *  @param flags the flags parameters inside the formatting part
     *  @param hasPrecision true if precision parameter is specified, false otherwise
     *  @param precision the index of the precision parameter inside the part
     *  @return reports an error
     *  if precision is specified or if the used flags are not allowed :
     *  ’d’: only ’#’ is allowed,
     *  ’o’, ’x’, ’X’: ’-’, ’#’, ’0’ are always allowed, depending on the type, this will be checked in the type check step
     */
    def checkIntegralConversion(partIndex : Int, argType : Option[Type], conversionChar : Char, flags : List[(Char, Int)], hasPrecision : Boolean, precision : Int) = {
      if (conversionChar == 'd')
        checkFlags(partIndex, flags, ('#', true,  "# not allowed for d conversion"))

      checkNotAllowedParameter(hasPrecision, partIndex, precision, "precision")
    }

    /** Checks all the formatting parameters for a Floating Point conversion
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param conversionChar the Char used for the formatting conversion
     *  @param flags the flags parameters inside the formatting part
     *  @param hasPrecision true if precision parameter is specified, false otherwise
     *  @param precision the index of the precision parameter inside the part
     *  @return reports an error
     *  if precision is specified for 'a', 'A' conversion or if the used flags are '(' and ',' for 'a', 'A'
     */
    def checkFloatingPointConversion(partIndex: Int, conversionChar : Char, flags : List[(Char, Int)], hasPrecision : Boolean, precision : Int) = {
      if(conversionChar == 'a' || conversionChar == 'A'){
        for {flag <- flags ; if (flag._1 == ',' || flag._1 == '(')}
          reporter.partError("'" + flag._1 + "' not allowed for a, A", partIndex, flag._2)
        checkNotAllowedParameter(hasPrecision, partIndex, precision, "precision")
      }
    }

    /** Checks all the formatting parameters for a Time conversion
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param part the part that we are checking
     *  @param conversionIndex the index of the conversion Char used in the part
     *  @param flags the flags parameters inside the formatting part
     *  @param hasPrecision true if precision parameter is specified, false otherwise
     *  @param precision the index of the precision parameter inside the part
     *  @return reports an error
     *  if precision is specified, if the time suffix is not given/incorrect or if the used flags are
     *  different from '-'
     */
    def checkTimeConversion(partIndex : Int, part : String, conversionIndex : Int, flags : List[(Char, Int)], hasPrecision : Boolean, precision : Int) = {
      /** Checks whether a time suffix is given and whether it is allowed
       *
       *  @param part the part that we are checking
       *  @param partIndex the index of the part inside of the parts of the StringContext
       *  @param conversionIndex the index of the conversion Char inside the part
       *  @param return reports an error if no suffix is specified or if the given suffix is not
       *  part of the allowed ones
       */
      def checkTime(part : String, partIndex : Int, conversionIndex : Int) : Unit = {
        if (conversionIndex + 1 >= part.size)
          reporter.partError("Date/time conversion must have two characters", partIndex, conversionIndex)
        else {
          part.charAt(conversionIndex + 1) match {
            case 'H' | 'I' | 'k' | 'l' | 'M' | 'S' | 'L' | 'N' | 'p' | 'z' | 'Z' | 's' | 'Q' => //times
            case 'B' | 'b' | 'h' | 'A' | 'a' | 'C' | 'Y' | 'y' | 'j' | 'm' | 'd' | 'e' => //dates
            case 'R' | 'T' | 'r' | 'D' | 'F' | 'c' => //dates and times
            case c => reporter.partError("'" + c + "' doesn't seem to be a date or time conversion", partIndex, conversionIndex + 1)
          }
        }
      }

      val notAllowedFlagOnCondition = for (flag <- List('#', '+', ' ', '0', ',', '(')) yield (flag, true, "Only '-' allowed for date/time conversions")
      checkUniqueFlags(partIndex, flags, notAllowedFlagOnCondition : _*)
      checkNotAllowedParameter(hasPrecision, partIndex, precision, "precision")
      checkTime(part, partIndex, conversionIndex)
    }

    /** Checks all the formatting parameters for a General conversion
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param argType the type of the argument matching with the given part
     *  @param conversionChar the Char used for the formatting conversion
     *  @param flags the flags parameters inside the formatting part
     *  @return reports an error
     *  if '#' flag is used or if any other flag is used
     */
    def checkGeneralConversion(partIndex : Int, argType : Option[Type], conversionChar : Char, flags : List[(Char, Int)]) = {
      for {flag <- flags ; if (flag._1 != '-' && flag._1 != '#')}
        reporter.partError("Illegal flag '" + flag._1 + "'", partIndex, flag._2)
    }

    /** Checks all the formatting parameters for a special Char such as '%' and end of line
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param conversionChar the Char used for the formatting conversion
     *  @param hasPrecision true if precision parameter is specified, false otherwise
     *  @param precision the index of the precision parameter inside the part
     *  @param hasWidth true if width parameter is specified, false otherwise
     *  @param width the index of the width parameter inside the part
     *  @return reports an error if precision or width is specified for '%' or
     *  if precision is specified for end of line
     */
    def checkSpecials(partIndex : Int, conversionChar : Char, hasPrecision : Boolean, precision : Int, hasWidth : Boolean, width : Int, flags : List[(Char, Int)]) = conversionChar match {
      case 'n' => {
        checkNotAllowedParameter(hasPrecision, partIndex, precision, "precision")
        checkNotAllowedParameter(hasWidth, partIndex, width, "width")
        val notAllowedFlagOnCondition = for (flag <- List('-', '#', '+', ' ', '0', ',', '(')) yield (flag, true, "flags not allowed")
        checkUniqueFlags(partIndex, flags, notAllowedFlagOnCondition : _*)
      }
      case '%' => {
        checkNotAllowedParameter(hasPrecision, partIndex, precision, "precision")
        val notAllowedFlagOnCondition = for (flag <- List('#', '+', ' ', '0', ',', '(')) yield (flag, true, "Illegal flag '" + flag + "'")
        checkFlags(partIndex, flags, notAllowedFlagOnCondition : _*)
      }
      case _ => // OK
    }

    /** Checks whether the format specifiers are correct depending on the conversion parameter
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param part the part to check
     *  The rest of the inputs correspond to the output of the function getFormatSpecifiers
     *  @param hasArgumentIndex
     *  @param actualArgumentIndex
     *  @param expectedArgumentIndex
     *  @param firstFormattingSubstring true if it is the first in the list, i.e. not an indexed argument
     *  @param maxArgumentIndex
     *  @param hasRelative
     *  @param hasWidth
     *  @param hasPrecision
     *  @param precision
     *  @param flags
     *  @param conversion
     *  @param argType
     *  @return the argument index and its type if there is an argument, the flags and the conversion parameter
     *  reports an error/warning if the formatting parameters are not allowed/wrong, nothing otherwise
     */
    def checkFormatSpecifiers(partIndex : Int, hasArgumentIndex : Boolean, actualArgumentIndex : Int, expectedArgumentIndex : Option[Int], firstFormattingSubstring : Boolean, maxArgumentIndex : Option[Int],
      hasRelative : Boolean, hasWidth : Boolean, width : Int, hasPrecision : Boolean, precision : Int, flags : List[(Char, Int)], conversion : Int, argType : Option[Type], part : String) : (Option[(Type, Int)], Char, List[(Char, Int)])= {
      val conversionChar = part.charAt(conversion)

      if (hasArgumentIndex && expectedArgumentIndex.nonEmpty && maxArgumentIndex.nonEmpty && firstFormattingSubstring)
        checkArgumentIndex(partIndex, actualArgumentIndex, hasRelative, part.charAt(actualArgumentIndex).asDigit, true, expectedArgumentIndex.get, maxArgumentIndex.get)
      else if(hasArgumentIndex && maxArgumentIndex.nonEmpty && !firstFormattingSubstring)
        checkArgumentIndex(partIndex, actualArgumentIndex, hasRelative, part.charAt(actualArgumentIndex).asDigit, false, 0, maxArgumentIndex.get)

      conversionChar match {
        case 'c' | 'C' => checkCharacterConversion(partIndex, flags, hasPrecision, precision)
        case 'd' | 'o' | 'x' | 'X' => checkIntegralConversion(partIndex, argType, conversionChar, flags, hasPrecision, precision)
        case 'e' | 'E' |'f' | 'g' | 'G' | 'a' | 'A' => checkFloatingPointConversion(partIndex, conversionChar, flags, hasPrecision, precision)
        case 't' | 'T' => checkTimeConversion(partIndex, part, conversion, flags, hasPrecision, precision)
        case 'b' | 'B' | 'h' | 'H' | 'S' | 's' => checkGeneralConversion(partIndex, argType, conversionChar, flags)
        case 'n' | '%' => checkSpecials(partIndex, conversionChar, hasPrecision, precision, hasWidth, width, flags)
        case illegal => reporter.partError("illegal conversion character '" + illegal + "'", partIndex, conversion)
      }

      (if (argType.isEmpty) None else Some(argType.get, (partIndex - 1)), conversionChar, flags)
    }

    /** Checks whether the argument type, if there is one, type checks with the formatting parameters
     *
     *  @param partIndex the index of the part, that we are checking, inside the parts
     *  @param conversionChar the character used for the conversion
     *  @param argument an option containing the type and index of the argument, None if there is no argument
     *  @param flags the flags used for the formatting
     *  @param formattingStart the index in the part where the formatting substring starts, i.e. where the '%' is
     *  @return reports an error/warning if the formatting parameters are not allowed/wrong depending on the type, nothing otherwise
     */
    def checkArgTypeWithConversion(partIndex : Int, conversionChar : Char, argument : Option[(Type, Int)], flags : List[(Char, Int)], formattingStart : Int) = {
      if (argument.nonEmpty)
        checkTypeWithArgs(argument.get, conversionChar, partIndex, flags)
      else
        checkTypeWithoutArgs(conversionChar, partIndex, flags, formattingStart)
    }

    /** Checks whether the argument type checks with the formatting parameters
     *
     *  @param argument the given argument to check
     *  @param conversionChar the conversion parameter inside the formatting String
     *  @param partIndex index of the part inside the String Context
     *  @param flags the list of flags, and their index, used inside the formatting String
     *  @return reports an error if the argument type does not correspond with the conversion character,
     *  nothing otherwise
     */
    def checkTypeWithArgs(argument : (Type, Int), conversionChar : Char, partIndex : Int, flags : List[(Char, Int)]) = {
      val booleans = List(definitions.BooleanType, definitions.NullType)
      val dates = List(definitions.LongType, typeOf[java.util.Calendar], typeOf[java.util.Date])
      val floatingPoints = List(definitions.DoubleType, definitions.FloatType, typeOf[java.math.BigDecimal])
      val integral = List(definitions.IntType, definitions.LongType, definitions.ShortType, definitions.ByteType, typeOf[java.math.BigInteger])
      val character = List(definitions.CharType, definitions.ByteType, definitions.ShortType, definitions.IntType)

      val (argType, argIndex) = argument
      conversionChar match {
        case 'c' | 'C' => checkSubtype(argType, "Char", argIndex, character : _*)
        case 'd' | 'o' | 'x' | 'X' => {
          checkSubtype(argType, "Int", argIndex, integral : _*)
          if (conversionChar != 'd') {
            val notAllowedFlagOnCondition = List(('+', !(argType <:< typeOf[java.math.BigInteger]), "only use '+' for BigInt conversions to o, x, X"),
            (' ', !(argType <:< typeOf[java.math.BigInteger]), "only use ' ' for BigInt conversions to o, x, X"),
            ('(', !(argType <:< typeOf[java.math.BigInteger]), "only use '(' for BigInt conversions to o, x, X"),
            (',', true, "',' only allowed for d conversion of integral types"))
            checkFlags(partIndex, flags, notAllowedFlagOnCondition : _*)
          }
        }
        case 'e' | 'E' |'f' | 'g' | 'G' | 'a' | 'A' => checkSubtype(argType, "Double", argIndex, floatingPoints : _*)
        case 't' | 'T' => checkSubtype(argType, "Date", argIndex, dates : _*)
        case 'b' | 'B' => checkSubtype(argType, "Boolean", argIndex, booleans : _*)
        case 'h' | 'H' | 'S' | 's' =>
          if (!(argType <:< typeOf[java.util.Formattable]))
            for {flag <- flags ; if (flag._1 == '#')}
              reporter.argError("type mismatch;\n found   : " + argType.widen.show.stripPrefix("scala.Predef.").stripPrefix("java.lang.").stripPrefix("scala.") + "\n required: java.util.Formattable", argIndex)
        case 'n' | '%' =>
        case illegal =>
      }
    }

    /** Reports error when the formatting parameter require a specific type but no argument is given
     *
     *  @param conversionChar the conversion parameter inside the formatting String
     *  @param partIndex index of the part inside the String Context
     *  @param flags the list of flags, and their index, used inside the formatting String
     *  @param formattingStart the index in the part where the formatting substring starts, i.e. where the '%' is
     *  @return reports an error if the formatting parameter refer to the type of the parameter but no parameter is given
     *  nothing otherwise
     */
    def checkTypeWithoutArgs(conversionChar : Char, partIndex : Int, flags : List[(Char, Int)], formattingStart : Int) = {
      conversionChar match {
          case 'o' | 'x' | 'X' => {
            val notAllowedFlagOnCondition = List(('+', true, "only use '+' for BigInt conversions to o, x, X"),
            (' ', true, "only use ' ' for BigInt conversions to o, x, X"),
            ('(', true, "only use '(' for BigInt conversions to o, x, X"),
            (',', true, "',' only allowed for d conversion of integral types"))
            checkFlags(partIndex, flags, notAllowedFlagOnCondition : _*)
          }
          case _ => //OK
        }
    }

    /** Checks that a given part of the String Context respects every formatting constraint per parameter
     *
     *  @param part a particular part of the String Context
     *  @param start the index from which we start checking the part
     *  @param argument an Option containing the argument corresponding to the part and its index in the list of args,
     *  None if no args are specified.
     *  @param maxArgumentIndex an Option containing the maximum argument index possible, None if no args are specified
     *  @return a list with all the elements of the conversion per formatting string
     */
    def checkPart(part : String, start : Int, argument : Option[(Int, Expr[Any])], maxArgumentIndex : Option[Int]) : List[(Option[(Type, Int)], Char, List[(Char, Int)])] = {
      reporter.resetReported()
      val hasFormattingSubstring = getFormattingSubstring(part, part.size, start)
      if (hasFormattingSubstring.nonEmpty) {
        val formattingStart = hasFormattingSubstring.get
        var nextStart = formattingStart

        argument match {
          case Some(argIndex, arg) => {
            val (hasArgumentIndex, argumentIndex, flags, hasWidth, width, hasPrecision, precision, hasRelative, relativeIndex, conversion) = getFormatSpecifiers(part, argIndex, argIndex + 1, false, formattingStart)
            if (!reporter.hasReported()){
              val conversionWithType = checkFormatSpecifiers(argIndex + 1, hasArgumentIndex, argumentIndex, Some(argIndex + 1), start == 0, maxArgumentIndex, hasRelative, hasWidth, width, hasPrecision, precision, flags, conversion, Some(arg.unseal.tpe), part)
              nextStart = conversion + 1
              conversionWithType :: checkPart(part, nextStart, argument, maxArgumentIndex)
            } else checkPart(part, conversion + 1, argument, maxArgumentIndex)
          }
          case None => {
            val (hasArgumentIndex, argumentIndex, flags, hasWidth, width, hasPrecision, precision, hasRelative, relativeIndex, conversion) = getFormatSpecifiers(part, 0, 0, true, formattingStart)
            if (hasArgumentIndex && !(part.charAt(argumentIndex).asDigit == 1 && (part.charAt(conversion) == 'n' || part.charAt(conversion) == '%')))
              reporter.partError("Argument index out of range", 0, argumentIndex)
            if (hasRelative)
              reporter.partError("No last arg", 0, relativeIndex)
            if (!reporter.hasReported()){
              val conversionWithType = checkFormatSpecifiers(0, hasArgumentIndex, argumentIndex, None, start == 0, maxArgumentIndex, hasRelative, hasWidth, width, hasPrecision, precision, flags, conversion, None, part)
              nextStart = conversion + 1
              if (!reporter.hasReported() && part.charAt(conversion) != '%' && part.charAt(conversion) != 'n' && !hasArgumentIndex && !hasRelative)
                reporter.partError("conversions must follow a splice; use %% for literal %, %n for newline", 0, part.indexOf('%'))
              conversionWithType :: checkPart(part, nextStart, argument, maxArgumentIndex)
            } else checkPart(part, conversion + 1, argument, maxArgumentIndex)
          }
        }
      } else {
        reporter.restoreReported()
        Nil
      }
    }

    val argument = args.size

    // check validity of formatting
    checkSizes(partsExpr.size - 1, argument)

    // add default format
    val parts = addDefaultFormat(partsExpr.map(literalToString))

    if (!parts.isEmpty && !reporter.hasReported()) {
      if (parts.size == 1 && args.size == 0 && parts.head.size != 0){
        val argTypeWithConversion = checkPart(parts.head, 0, None, None)
        if (!reporter.hasReported())
          for ((argument, conversionChar, flags) <- argTypeWithConversion)
            checkArgTypeWithConversion(0, conversionChar, argument, flags, parts.head.indexOf('%'))
      } else {
        val partWithArgs = parts.tail.zip(args)
        for (i <- (0 until args.size)){
          val (part, arg) = partWithArgs(i)
          val argTypeWithConversion = checkPart(part, 0, Some((i, arg)), Some(args.size))
          if (!reporter.hasReported())
            for ((argument, conversionChar, flags) <- argTypeWithConversion)
              checkArgTypeWithConversion(i + 1, conversionChar, argument, flags, parts(i).indexOf('%'))
        }
      }
    }

    // macro expansion
    '{(${parts.mkString.toExpr}).format(${argsExpr}: _*)}
  }
}