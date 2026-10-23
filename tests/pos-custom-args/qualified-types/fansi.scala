// Benchmark from OOPSLA 26
// Source: https://github.com/com-lihaoyi/fansi
// Modified: refinement type `ChannelValue` instead of `require` checks

package fansi

import java.util

import scala.language.implicitConversions

import scala.annotation.tailrec
import scala.collection.mutable

object sourcecode:
  class Name(val value: String)
  given Name = Name("")

object sourceName:
  val value = ""

/**
  * Encapsulates a string with associated ANSI colors and text decorations.
  *
  * This is your primary data-type when you are dealing with colored fansi
  * strings.
  *
  * Contains some basic string methods, as well as some ansi methods to e.g.
  * apply particular colors or other decorations to particular sections of
  * the [[fansi.Str]]. [[render]] flattens it out into a `java.lang.String`
  * with all the colors present as ANSI escapes.
  *
  * Avoids using Scala collections operations in favor of util.Arrays,
  * giving 20% (on `++`) to >1000% (on `splitAt`, `subString`
  * and `Str.parse`) speedups
  */
class Str private(private val chars: Array[Char], private val colors: Array[Str.State]) {
  require(chars.length == colors.length)
  override def hashCode() = util.Arrays.hashCode(chars) + util.Arrays.hashCode(colors)
  override def equals(other: Any) = other match{
    case o: fansi.Str =>
      util.Arrays.equals(chars, o.chars) && util.Arrays.equals(colors, o.colors)
    case _ => false
  }
  /**
    * Concatenates two [[fansi.Str]]s, preserving the colors in each one and
    * avoiding any interference between them
    */
  def ++(other: Str) = {
    val chars2 = new Array[Char](length + other.length)
    val colors2 = new Array[Str.State](length + other.length)
    System.arraycopy(chars, 0, chars2, 0, length)
    System.arraycopy(other.chars, 0, chars2, length, other.length)
    System.arraycopy(colors, 0, colors2, 0, length)
    System.arraycopy(other.colors, 0, colors2, length, other.length)

    new Str(chars2, colors2)
  }

  /**
    * Splits an [[fansi.Str]] into two sub-strings, preserving the colors in
    * each one.
    *
    * @param index the plain-text index of the point within the [[fansi.Str]]
    *              you want to use to split it.
    */
  def splitAt(index: Int) = (
    new Str(
      util.Arrays.copyOfRange(chars, 0, index),
      util.Arrays.copyOfRange(colors, 0, index)
    ),
    new Str(
      util.Arrays.copyOfRange(chars, index, length),
      util.Arrays.copyOfRange(colors, index, length)
    )
  )

  /**
    * Returns an [[fansi.Str]] which is a substring of this string,
    * and has the same colors as the original section of this string
    * did
    */
  def substring(start: Int = 0, end: Int = length) = {
    require(start >= 0 && start <= length,
      s"substring start parameter [$start] must be between 0 and length:$length"
    )
    require(end >= start && end <= length,
      s"substring end parameter [$end] must be between start $start and length:$length"
    )
    new Str(
      util.Arrays.copyOfRange(chars, start, end),
      util.Arrays.copyOfRange(colors, start, end)
    )
  }



  /**
    * The plain-text length of this [[fansi.Str]], in UTF-16 characters (same
    * as `.length` on a `java.lang.String`). If you want fancy UTF-8 lengths,
    * use `.plainText`
    */
  def length = chars.length


  override def toString = render

  /**
    * The plain-text `java.lang.String` represented by this [[fansi.Str]],
    * without all the fansi colors or other decorations
    */
  lazy val plainText = new String(chars)

  /**
    * Returns a copy of the colors array backing this `fansi.Str`, in case
    * you want to use it to
    */
  def getColors = colors.clone()

  /**
    * Retrieve the color of this string at the given character index
    */
  def getColor(i: Int) = colors(i)
  /**
    * Returns a copy of the character array backing this `fansi.Str`, in case
    * you want to use it to
    */
  def getChars = chars.clone()
  /**
    * Retrieve the character of this string at the given character index
    */
  def getChar(i: Int) = chars(i)

  /**
    * Converts this [[fansi.Str]] into a `java.lang.String`, including all
    * the fancy fansi colors or decorations as fansi escapes embedded within
    * the string. "Terminates" colors at the right-most end of the resultant
    * `java.lang.String`, making it safe to concat-with or embed-inside other
    * `java.lang.String` without worrying about fansi colors leaking out of it.
    */
  def render = {
    // Pre-size StringBuilder with approximate size (ansi colors tend
    // to be about 5 chars long) to avoid re-allocations during growth
    val output = new StringBuilder(chars.length + colors.length * 5)


    var currentState: Str.State = 0

    // Make a local array copy of the immutable Vector, for maximum performance
    // since the Vector is small and we'll be looking it up over & over & over
    val categoryArray = Attr.categories.toArray

    var i = 0
    while(i < colors.length){
      // Emit ANSI escapes to change colors where necessary
      // fast-path optimization to check for integer equality first before
      // going through the whole `enableDiff` rigmarole
      if (colors(i) != currentState) {
        Attrs.emitAnsiCodes0(currentState, colors(i), output, categoryArray)
        currentState = colors(i)
      }
      output.append(chars(i))
      i += 1
    }

    // Cap off the left-hand-side of the rendered string with any ansi escape
    // codes necessary to rest the state to 0
    Attrs.emitAnsiCodes0(currentState, 0, output, categoryArray)

    output.toString
  }



  /**
    * Overlays the desired color over the specified range of the [[fansi.Str]].
    */
  def overlay(attrs: Attrs, start: Int = 0, end: Int = length) = {
    overlayAll(Seq((attrs, start, end)))
  }

  /**
    * Batch version of [[overlay]], letting you apply a bunch of [[Attrs]] onto
    * various parts of the same string in one operation, avoiding the unnecessary
    * copying that would happen if you applied them with [[overlay]] one by one.
    *
    * The input sequence of overlay-tuples is applied from left to right
    */
  def overlayAll(overlays: Seq[(Attrs, Int, Int)]) = {
    val colorsOut = colors.clone()
    for((attrs, start, end) <- overlays){
      require(
        end >= start,
        s"end:$end must be greater than start:$end in fansiStr#overlay call"
      )
      require(start >= 0, s"start:$start must be greater than or equal to 0")
      require(
        end <= colors.length,
        s"end:$end must be less than or equal to length:${colors.length}"
      )

      {
        var i = start
        while (i < end) {
          colorsOut(i) = attrs.transform(colorsOut(i))
          i += 1
        }
      }
    }
    new Str(chars, colorsOut)
  }
}

object Str{

  /**
    * An [[fansi.Str]]'s `color`s array is filled with Long, each representing
    * the ANSI state of one character encoded in its bits. Each [[Attr]] belongs
    * to a [[Category]] that occupies a range of bits within each long:
    *
    * 61... 55 54  53 52 51 .... 31 30 29 28  27 26 25 ..... 6  5  4  3  2  1  0
    *  |--------|  |-----------------------|  |-----------------------|  |  |  |bold
    *           |                          |                          |  |  |reversed
    *           |                          |                          |  |underlined
    *           |                          |                          |foreground-color
    *           |                          |background-color
    *           |unused
    *
    *
    * The `0000 0000 0000 0000` long corresponds to plain text with no decoration
    *
    */
  type State = Long

  /**
    * Make the construction of [[fansi.Str]]s from `String`s and other
    * `CharSequence`s automatic
    */
  implicit def implicitApply(raw: CharSequence): fansi.Str = apply(raw)

  /**
    * Regex that can be used to identify Ansi escape patterns in a string.
    *
    *
    *
    * Found from: http://stackoverflow.com/a/33925425/871202
    *
    * Which references:
    *
    * http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-048.pdf
    *
    * Section 5.4: Control Sequences
    */
  val ansiRegex = "(\u009b|\u001b\\[)[0-?]*[ -\\/]*[@-~]".r.pattern

  /** Shorthand constructor with ErrorMode.Sanitize */
  def Sanitize(raw: CharSequence) = apply(raw, ErrorMode.Sanitize)

  /** Shorthand constructor with ErrorMode.Strip */
  def Strip(raw: CharSequence) = apply(raw, ErrorMode.Strip)

  /** Shorthand constructor with ErrorMode.Throw */
  def Throw(raw: CharSequence) = apply(raw, ErrorMode.Throw)
  /**
    * Creates an [[fansi.Str]] from a non-fansi `java.lang.String` or other
    * `CharSequence`.
    *
    * Note that this method is implicit, meaning you can pass in a
    * `java.lang.String` anywhere an `fansi.Str` is required and it will be
    * automatically parsed and converted for you.
    *
    * @param errorMode Used to control what kind of behavior you get if the
    *                  input `CharSequence` contains an Ansi escape not
    *                  recognized by Fansi as a valid color.
    */
  def apply(raw: CharSequence, errorMode: ErrorMode = ErrorMode.Sanitize): fansi.Str = {
    // Pre-allocate some arrays for us to fill up. They will probably be
    // too big if the input has any ansi codes at all but that's ok, we'll
    // trim them later.
    val chars = new Array[Char](raw.length)
    val colors = new Array[Str.State](raw.length)

    var currentColor = 0l
    var sourceIndex = 0
    var destIndex = 0
    val length = raw.length
    while(sourceIndex < length){
      val char = raw.charAt(sourceIndex)
      if (char == '\u001b' || char == '\u009b') {
        val escapeStartSourceIndex = sourceIndex
        ParseMap.query(raw, escapeStartSourceIndex) match{
          case None => sourceIndex = errorMode.handle(sourceIndex, raw)
          case Some(tuple) =>
            tuple match {
              case (newIndex, Left(color)) =>
                currentColor = color.transform(currentColor)
                sourceIndex += newIndex
              case (newIndex, Right(category)) =>
                // Gross manual char-by-char parsing of the remainder
                // of the True-color escape, to maximize performance
                sourceIndex += newIndex
                def isDigit(index: Int) = {
                  index < raw.length && raw.charAt(index) >= '0' && raw.charAt(index) <= '9'
                }
                def checkChar(index: Int, char: Char) = {
                  index < raw.length && raw.charAt(index) == char
                }
                def fail() = {
                  sourceIndex = errorMode.handle(escapeStartSourceIndex, raw)
                }
                def getNumber() = {
                  var value = 0
                  var count = 0
                  while (isDigit(sourceIndex) && count < 3) {
                    value = value * 10 + (raw.charAt(sourceIndex) - '0').toInt
                    sourceIndex += 1
                    count += 1
                  }
                  value
                }
                if (!isDigit(sourceIndex)) fail()
                else {
                  val r = getNumber()
                  if (!checkChar(sourceIndex, ';') || !isDigit(sourceIndex + 1)) fail()
                  else {
                    sourceIndex += 1
                    val g = getNumber()
                    if (!checkChar(sourceIndex, ';') || !isDigit(sourceIndex + 1)) fail()
                    else {
                      sourceIndex += 1
                      val b = getNumber()
                      if (!checkChar(sourceIndex, 'm')) fail()
                      else {
                        sourceIndex += 1
                        // Manually perform the `transform` for perf to avoid
                        // calling `True` which instantiates/allocaties an `Attr`
                        if(0 <= r && r < 256 && 0 <= g && g < 256 && 0 <= b && b < 256)
                          currentColor = {
                            (currentColor & ~category.mask) |
                              ((273 + category.trueIndex(r, g, b)) << category.offset)
                          }
                        else{
                          fail()
                        }
                      }
                    }
                  }
                }
            }
        }
      }else {
        colors(destIndex) = currentColor
        chars(destIndex) = char
        sourceIndex += 1
        destIndex += 1
      }
    }

    new Str(
      util.Arrays.copyOfRange(chars, 0, destIndex),
      util.Arrays.copyOfRange(colors, 0, destIndex)
    )
  }

  /**
    * Constructs a [[fansi.Str]] from an array of characters and an array
    * of colors. Performs a defensive copy of the arrays, and validates that
    * they both have the same length
    *
    * Useful together with `getChars` and `getColors` if you want to do manual
    * work on the two mutable arrays before stitching them back together into
    * one immutable [[fansi.Str]]
    */
  def fromArrays(chars: Array[Char], colors: Array[Str.State]) = {
    new fansi.Str(chars.clone(), colors.clone())
  }

  def apply(args: Str*): fansi.Str = {
    join(args)
  }
  def join(args: Iterable[Str], sep: fansi.Str = fansi.Str("")) = {
    val length = math.max(0, args.iterator.map(_.length + sep.length).sum - sep.length)
    val chars = new Array[Char](length)
    val colors = new Array[State](length)
    var j = 0
    for (arg <- args){

      if (j != 0){
        var k = 0
        while (k < sep.length){
          chars(j) = sep.getChar(k)
          colors(j) = sep.getColors(k)
          j += 1
          k += 1
        }
      }
      var i = 0
      while (i < arg.length){
        chars(j) = arg.getChar(i)
        colors(j) = arg.getColor(i)
        i += 1
        j += 1
      }
    }
    fromArrays(chars, colors)
  }
  private val ParseMap = {
    val pairs = for {
      cat <- Attr.categories
      color <- cat.all
      str <- color.escapeOpt
    } yield (str, Left(color))
    val reset = Seq(
      Console.RESET -> Left(Attr.Reset)
    )
    val trueColors = Seq(
      "\u001b[38;2;" -> Right(Color),
      "\u001b[48;2;" -> Right(Back)
    )
    new Trie(pairs ++ reset ++ trueColors)
  }
}


/**
  * Used to control what kind of behavior you get if the a `CharSequence` you
  * are trying to parse into a [[fansi.Str]] contains an Ansi escape not
  * recognized by Fansi as a valid color.
  */
sealed trait ErrorMode{
  /**
    * Given an unknown Ansi escape was found at `sourceIndex` inside your
    * `raw: CharSequence`, what index should you resume parsing at?
    */
  def handle(sourceIndex: Int, raw: CharSequence): Int
}
object ErrorMode{
  /**
    * Throw an exception and abort the parse
    */
  case object Throw extends ErrorMode{
    def handle(sourceIndex: Int, raw: CharSequence) = {
      val matcher = Str.ansiRegex.matcher(raw)
      val detail =
        if (!matcher.find(sourceIndex)) ""
        else {
          val end = matcher.end()
          " " + raw.subSequence(sourceIndex + 1, end)
        }

      throw new IllegalArgumentException(
        s"Unknown ansi-escape$detail at index $sourceIndex " +
          "inside string cannot be parsed into an fansi.Str"
      )
    }
  }
  /**
    * Skip the `\u001b` that kicks off the unknown Ansi escape but leave
    * subsequent characters in place, so the end-user can see that an Ansi
    * escape was entered e.g. via the [A[B[A[C that appears in the result
    */
  case object Sanitize extends ErrorMode{
    def handle(sourceIndex: Int, raw: CharSequence) = {
      sourceIndex + 1
    }
  }

  /**
    * Find the end of the unknown Ansi escape and skip over it's characters
    * entirely, so no trace of them appear in the parsed fansi.Str.
    */
  case object Strip extends ErrorMode{
    def handle(sourceIndex: Int, raw: CharSequence) = {
      val matcher = Str.ansiRegex.matcher(raw)
      matcher.find(sourceIndex)
      matcher.end()
    }
  }
}

/**
  * Represents one or more [[fansi.Attr]]s, that can be passed around
  * as a set or combined with other sets of [[fansi.Attr]]s.
  *
  * Note that a single [[Attr]] is a subclass of [[Attrs]]. If you want to
  * know if this contains multiple [[Attr]]s, you should check for
  * [[Attrs.Multiple]].
  */
sealed trait Attrs{

  /**
    * Apply these [[Attrs]] to the given [[fansi.Str]], making it take effect
    * across the entire length of that string.
    */
  def apply(s: fansi.Str) = s.overlay(this, 0, s.length)

  /**
    * Which bits of the [[Str.State]] integer these [[Attrs]] will
    * override when it is applied
    */
  def resetMask: Long

  /**
    * Which bits of the [[Str.State]] integer these [[Attrs]] will
    * set to `1` when it is applied
    */
  def applyMask: Long

  /**
    * Apply the current [[Attrs]] to the [[Str.State]] integer,
    * modifying it to represent the state after all changes have taken
    * effect
    */
  def transform(state: Str.State) = (state & ~resetMask) | applyMask

  /**
    * Combine this [[fansi.Attrs]] with other [[fansi.Attrs]]s, returning one
    * which when applied is equivalent to applying this one and then the `other`
    * one in series.
    */
  def ++(other: fansi.Attrs): fansi.Attrs

}

object Attrs{

  val Empty = Attrs()

  /**
    * Emit the ansi escapes necessary to transition
    * between two states, if necessary, as a `java.lang.String`
    */
  def emitAnsiCodes(currentState: Str.State, nextState: Str.State) = {
    val output = new StringBuilder
    val categoryArray = Attr.categories.toArray
    emitAnsiCodes0(currentState, nextState, output, categoryArray)
    output.toString
  }

  /**
    * Messy-but-fast version of [[emitAnsiCodes]] that avoids allocating things
    * unnecessarily. Reads it's category listing from a fast Array version of
    * Attrs.categories and writes it's output to a mutable `StringBuilder`
    */
  def emitAnsiCodes0(currentState: Str.State,
                nextState: Str.State,
                output: StringBuilder,
                categoryArray: Array[Category]) = {
    if (currentState != nextState){

      val hardOffMask = Bold.mask
      // Any of these transitions from 1 to 0 within the hardOffMask
      // categories cannot be done with a single ansi escape, and need
      // you to emit a RESET followed by re-building whatever ansi state
      // you previous had from scratch
      val currentState2 =
        if ((currentState & ~nextState & hardOffMask) != 0){
          output.append(Console.RESET)
          0l
        }else {
          currentState
        }

      var categoryIndex = 0
      while(categoryIndex < categoryArray.length){
        val cat = categoryArray(categoryIndex)
        if ((cat.mask & currentState2) != (cat.mask & nextState)){
          val escape = cat.lookupEscape(nextState & cat.mask)
          output.append(escape)
        }
        categoryIndex += 1
      }
    }
  }

  def apply(attrs: Attr*): Attrs = {
    var output = List.empty[Attr]
    var resetMask = 0l
    var applyMask = 0l
    // Walk the list of attributes backwards, and aggregate only those whose
    // `resetMask` is not going to get totally covered by the union of all
    // `resetMask`s that come after it.
    //
    // Simultaneously build up the `applyMask`, which is the `applyMask` of
    // all aggregated `attr`s whose own `applyMask` is not totally covered by
    // the union of all `resetMask`s that come after.
    for(attr <- attrs.reverseIterator){
      if ((attr.resetMask & ~resetMask) != 0){
        if ((attr.applyMask & resetMask) == 0) applyMask = applyMask | attr.applyMask
        resetMask = resetMask | attr.resetMask
        output = attr :: output
      }
    }

    if (output.length == 1) output.head
    else new Multiple(resetMask, applyMask, output.toArray.reverse*)
  }

  class Multiple private[Attrs] (val resetMask: Long,
                                 val applyMask: Long,
                                 val attrs: Attr*) extends Attrs{
    assert(attrs.length != 1)
    override def hashCode() = attrs.hashCode()
    override def equals(other: Any) = (this, other) match{
      case (lhs: Attr, rhs: Attr) => lhs eq rhs
      case (lhs: Attr, rhs: Attrs.Multiple) if rhs.attrs.length == 1 => lhs eq rhs.attrs(0)
      case (lhs: Attrs.Multiple, rhs: Attr) if lhs.attrs.length == 1 => lhs.attrs(0) eq rhs
      case (lhs: Attrs.Multiple, rhs: Attrs.Multiple) => lhs.attrs eq rhs.attrs
      case _ => false
    }

    override def toString = s"Attrs(${attrs.mkString(",")})"

    def ++(other: fansi.Attrs) = Attrs(attrs ++ toSeq(other)*)
  }
  def toSeq(attrs: Attrs) = attrs match{
    case m: Multiple => m.attrs
    case a: Attr => Seq(a)
  }
}
/**
  * Represents a single, atomic ANSI escape sequence that results in a
  * color, background or decoration being added to the output. May or may not
  * have an escape sequence (`escapeOpt`), as some attributes (e.g. [[Bold.Off]])
  * are not widely/directly supported by terminals and so fansi.Str supports them
  * by rendering a hard [[Attr.Reset]] and then re-rendering other [[Attr]]s that are
  * active.
  *
  * Many of the codes were stolen shamelessly from
  *
  * http://misc.flogisoft.com/bash/tip_colors_and_formatting
  */
sealed trait Attr extends Attrs {
  def attrs = Seq(this)
  /**
    * escapeOpt the actual ANSI escape sequence corresponding to this Attr
    */
  def escapeOpt: Option[String]

  def name: String

  /**
    * Combine this [[fansi.Attr]] with one or more other [[fansi.Attr]]s
    * so they can be passed around together
    */
  def ++(other: fansi.Attrs): Attrs = Attrs(Array(this) ++ Attrs.toSeq(other)*)
}
object Attr{
  /**
    * Represents the removal of all ansi text decoration. Doesn't fit into any
    * convenient category, since it applies to them all.
    */
  val Reset = new EscapeAttr(Console.RESET, Int.MaxValue, 0)

  /**
    * A list of possible categories
    */
  val categories = Vector[Category](
    Color,
    Back,
    Bold,
    Underlined,
    Reversed
  )
}
/**
  * An [[Attr]] represented by an fansi escape sequence
  */
case class EscapeAttr private[fansi](escape: String, resetMask: Long, applyMask: Long) extends Attr{
  val escapeOpt = Some(escape)
  val name = sourceName.value
  override def toString = escape + name + Console.RESET
}

/**
  * An [[Attr]] for which no fansi escape sequence exists
  */
case class ResetAttr private[fansi](resetMask: Long, applyMask: Long) extends Attr{
  val escapeOpt = None
  val name = sourceName.value
  override def toString = name
}



/**
  * Represents a set of [[fansi.Attr]]s all occupying the same bit-space
  * in the state `Int`
  */
sealed abstract class Category(val offset: Int, val width: Int)(using catName: sourcecode.Name){
  def mask = ((1 << width) - 1) << offset
  val all: Vector[Attr]

  def lookupEscape(applyState: Long) = {
    val escapeOpt = lookupAttr(applyState).escapeOpt
    if (escapeOpt.isDefined) escapeOpt.get
    else ""
  }
  def lookupAttr(applyState: Long) = lookupAttrTable((applyState >> offset).toInt)

  // Allows fast lookup of categories based on the desired applyState
  protected def lookupTableWidth = 1 << width

  protected lazy val lookupAttrTable = {
    val arr = new Array[Attr](lookupTableWidth)
    for(attr <- all){
      arr((attr.applyMask >> offset).toInt) = attr
    }
    arr
  }

  def makeAttr(s: String, applyValue: Long)(using name: sourcecode.Name) = {
    ???
  }

  def makeNoneAttr(applyValue: Long)(using name: sourcecode.Name) = {
    ???
  }
}

/**
  * [[Attr]]s to turn text bold/bright or disable it
  */
object Bold extends Category(offset = 0, width = 2){
  val Faint = makeAttr("\u001b[2m",  2)(using sourcecode.Name("Faint"))
  val On  = makeAttr(Console.BOLD, 1)
  val Off = makeNoneAttr(          0)
  val all: Vector[Attr] = Vector(On, Off, Faint)
}

/**
  * [[Attr]]s to reverse the background/foreground colors of your text,
  * or un-reverse them
  */
object Reversed extends Category(offset = 2, width = 1){
  val On  = makeAttr(Console.REVERSED,   1)
  val Off = makeAttr("\u001b[27m",       0)
  val all: Vector[Attr] = Vector(On, Off)
}
/**
  * [[Attr]]s to enable or disable underlined text
  */
object Underlined extends Category(offset = 3, width = 1){
  val On  = makeAttr(Console.UNDERLINED, 1)
  val Off = makeAttr("\u001b[24m",       0)
  val all: Vector[Attr] = Vector(On, Off)
}

/**
  * [[Attr]]s to set or reset the color of your foreground text
  */
object Color extends ColorCategory(offset = 4, width = 25, colorCode = 38){

  val Reset        = makeAttr("\u001b[39m",     0)
  val Black        = makeAttr(Console.BLACK,    1)
  val Red          = makeAttr(Console.RED,      2)
  val Green        = makeAttr(Console.GREEN,    3)
  val Yellow       = makeAttr(Console.YELLOW,   4)
  val Blue         = makeAttr(Console.BLUE,     5)
  val Magenta      = makeAttr(Console.MAGENTA,  6)
  val Cyan         = makeAttr(Console.CYAN,     7)
  val LightGray    = makeAttr("\u001b[37m",     8)
  val DarkGray     = makeAttr("\u001b[90m",     9)
  val LightRed     = makeAttr("\u001b[91m",    10)
  val LightGreen   = makeAttr("\u001b[92m",    11)
  val LightYellow  = makeAttr("\u001b[93m",    12)
  val LightBlue    = makeAttr("\u001b[94m",    13)
  val LightMagenta = makeAttr("\u001b[95m",    14)
  val LightCyan    = makeAttr("\u001b[96m",    15)
  val White        = makeAttr("\u001b[97m",    16)

  val all: Vector[Attr] = Vector(
    Reset, Black, Red, Green, Yellow, Blue, Magenta, Cyan, LightGray, DarkGray,
    LightRed, LightGreen, LightYellow, LightBlue, LightMagenta, LightCyan, White
  ) ++ Full

}

/**
  * [[Attr]]s to set or reset the color of your background
  */
object Back extends ColorCategory(offset = 29, width = 25, colorCode = 48){

  val Reset        = makeAttr("\u001b[49m",       0)
  val Black        = makeAttr(Console.BLACK_B,    1)
  val Red          = makeAttr(Console.RED_B,      2)
  val Green        = makeAttr(Console.GREEN_B,    3)
  val Yellow       = makeAttr(Console.YELLOW_B,   4)
  val Blue         = makeAttr(Console.BLUE_B,     5)
  val Magenta      = makeAttr(Console.MAGENTA_B,  6)
  val Cyan         = makeAttr(Console.CYAN_B,     7)
  val LightGray    = makeAttr("\u001b[47m",       8)
  val DarkGray     = makeAttr("\u001b[100m",      9)
  val LightRed     = makeAttr("\u001b[101m",     10)
  val LightGreen   = makeAttr("\u001b[102m",     11)
  val LightYellow  = makeAttr("\u001b[103m",     12)
  val LightBlue    = makeAttr("\u001b[104m",     13)
  val LightMagenta = makeAttr("\u001b[105m",     14)
  val LightCyan    = makeAttr("\u001b[106m",     15)
  val White        = makeAttr("\u001b[107m",     16)


  val all: Vector[Attr] = Vector(
    Reset, Black, Red, Green, Yellow, Blue, Magenta, Cyan, LightGray, DarkGray,
    LightRed, LightGreen, LightYellow, LightBlue, LightMagenta, LightCyan, White
  ) ++ Full
}


/**
  * An string trie for quickly looking up values of type [[T]]
  * using string-keys. Used to speed up
  */
private final class Trie[T](strings: Seq[(String, T)]){

  val (min, max, arr, value) = {
    strings.partition(_._1.isEmpty) match{
      case (Nil, continuations) =>
        val allChildChars = continuations.map(_._1(0))
        val min = allChildChars.min
        val max = allChildChars.max

        val arr = new Array[Trie[T]](max - min + 1)
        for( (char, ss) <- continuations.groupBy(_._1(0)) ){
          arr(char - min) = new Trie(ss.map{case (k, v) => (k.tail, v)})
        }

        (min, max, arr, None)

      case (Seq((_, terminalValue)), Nil) =>
        (
          0.toChar,
          0.toChar,
          new Array[Trie[T]](0),
          Some(terminalValue)
        )

      case _ => ???
    }
  }

  def apply(c: Char): Trie[T] = {
    if (c > max || c < min) null
    else arr(c - min)
  }

  /**
    * Returns the length of the matching string, or -1 if not found
    */
  def query(input: CharSequence, index: Int): Option[(Int, T)] = {

    @tailrec def rec(offset: Int, currentNode: Trie[T]): Option[(Int, T)] = {

      if (currentNode.value.isDefined) currentNode.value.map(offset - index -> _)
      else if (offset >= input.length) None
      else {
        val char = input.charAt(offset)
        val next = currentNode(char)
        if (next == null) None
        else rec(offset + 1, next)
      }
    }
    rec(index, this)
  }
}


/**
  * * Color a encoded on 25 bit as follow :
  * 0 : reset value
  * 1 - 16 : 3 bit colors
  * 17 - 272 : 8 bit colors
  * 273 - 16 777 388 : 24 bit colors
  */
abstract class ColorCategory(offset: Int, width: Int, val colorCode: Int)
                            (using catName: sourcecode.Name)
                             extends Category (offset, width){



  /**
    * 256 color [[Attr]]s, for those terminals that support it
    */
  val Full =
    for(x <- 0 until 256)
    yield makeAttr(s"\u001b[$colorCode;5;${x}m", 17 + x)(using sourcecode.Name(s"Full($x)"))

  private def True0(r: Int, g: Int, b: Int, index: Int) = {
    makeAttr(trueRgbEscape(r, g, b), 273 + index)(using sourcecode.Name("True(" + r + "," + g + "," + b +")"))
  }
  def trueRgbEscape(r: Int, g: Int, b: Int) = {
    "\u001b[" + colorCode + ";2;" + r + ";" + g + ";" + b + "m"
  }

  /**
    * Create a TrueColor color, from a given index within the 16-million-color
    * TrueColor range
    */
  def True(index: Int) = {
    require(
      0 <= index && index <= (1 << 24),
      "True parameter `index` must be 273 <= index <= 16777488, not " + index
    )
    val r = index >> 16
    val g = (index & 0x00FF00) >> 8
    val b = index & 0x0000FF
    True0(r, g, b, index)
  }

  type ChannelValue = {v: Int with 0 <= v && v < 256}

  /**
    * Create a TrueColor color, from a given (r, g, b) within the 16-million-color
    * TrueColor range
    */
  def True(r: ChannelValue, g: ChannelValue, b: ChannelValue) = True0(r, g, b, trueIndex(r, g, b))

  def trueIndex(r: ChannelValue, g: ChannelValue, b: ChannelValue) = {
    r << 16 | g << 8 | b
  }

  override def lookupEscape(applyState : Long) = {
    val rawIndex = (applyState >> offset).toInt
    if(rawIndex < 273) super.lookupEscape(applyState)
    else {
      val index = rawIndex - 273
      trueRgbEscape(r = index >> 16, g = (index & 0x00FF00) >> 8, b = index & 0x0000FF)
    }
  }
  override def lookupAttr(applyState : Long): Attr = {
    val index = (applyState >> offset).toInt
    if(index < 273) lookupAttrTable(index)
    else True(index - 273)

  }
  override protected def lookupTableWidth = 273
}
