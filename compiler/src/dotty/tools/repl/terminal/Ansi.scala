package dotty.tools
package repl
package terminal

object Ansi {

  /**
    * Represents a single, atomic ANSI escape sequence that results in a
    * color, background or decoration being added to the output.
    *
    * @param escape the actual ANSI escape sequence corresponding to this Attr
    */
  case class Attr private[Ansi](escape: Option[String], resetMask: Int, applyMask: Int) {
    override def toString = escape.getOrElse("") + Console.RESET
    def transform(state: Short) = ((state & ~resetMask) | applyMask).toShort

    def matches(state: Short) = (state & resetMask) == applyMask
    def apply(s: Ansi.Str) = s.overlay(this, 0, s.length)
  }

  object Attr {
    val Reset = new Attr(Some(Console.RESET), Short.MaxValue, 0)

    /**
      * Quickly convert string-colors into [[Ansi.Attr]]s
      */
    val ParseMap = {
      val pairs = for {
        cat <- categories
        color <- cat.all
        str <- color.escape
      } yield (str, color)
      (pairs :+ (Console.RESET -> Reset)).toMap
    }
  }

  /**
    * Represents a set of [[Ansi.Attr]]s all occupying the same bit-space
    * in the state `Short`
    */
  sealed abstract class Category() {
    val mask: Int
    val all: Seq[Attr]
    lazy val bitsMap = all.map{ m => m.applyMask -> m}.toMap
    def makeAttr(s: Option[String], applyMask: Int) = {
      new Attr(s, mask, applyMask)
    }
  }

  object Color extends Category {

    val mask = 15 << 7
    val Reset     = makeAttr(Some("\u001b[39m"),     0 << 7)
    val Black     = makeAttr(Some(Console.BLACK),    1 << 7)
    val Red       = makeAttr(Some(Console.RED),      2 << 7)
    val Green     = makeAttr(Some(Console.GREEN),    3 << 7)
    val Yellow    = makeAttr(Some(Console.YELLOW),   4 << 7)
    val Blue      = makeAttr(Some(Console.BLUE),     5 << 7)
    val Magenta   = makeAttr(Some(Console.MAGENTA),  6 << 7)
    val Cyan      = makeAttr(Some(Console.CYAN),     7 << 7)
    val White     = makeAttr(Some(Console.WHITE),    8 << 7)

    val all = Vector(
      Reset, Black, Red, Green, Yellow,
      Blue, Magenta, Cyan, White
    )
  }

  object Back extends Category {
    val mask = 15 << 3

    val Reset    = makeAttr(Some("\u001b[49m"),       0 << 3)
    val Black    = makeAttr(Some(Console.BLACK_B),    1 << 3)
    val Red      = makeAttr(Some(Console.RED_B),      2 << 3)
    val Green    = makeAttr(Some(Console.GREEN_B),    3 << 3)
    val Yellow   = makeAttr(Some(Console.YELLOW_B),   4 << 3)
    val Blue     = makeAttr(Some(Console.BLUE_B),     5 << 3)
    val Magenta  = makeAttr(Some(Console.MAGENTA_B),  6 << 3)
    val Cyan     = makeAttr(Some(Console.CYAN_B),     7 << 3)
    val White    = makeAttr(Some(Console.WHITE_B),    8 << 3)

    val all = Seq(
      Reset, Black, Red, Green, Yellow,
      Blue, Magenta, Cyan, White
    )
  }

  object Bold extends Category {
    val mask = 1 << 0
    val On  = makeAttr(Some(Console.BOLD), 1 << 0)
    val Off = makeAttr(None              , 0 << 0)
    val all = Seq(On, Off)
  }

  object Underlined extends Category {
    val mask = 1 << 1
    val On  = makeAttr(Some(Console.UNDERLINED), 1 << 1)
    val Off = makeAttr(None,                     0 << 1)
    val all = Seq(On, Off)
  }

  object Reversed extends Category {
    val mask = 1 << 2
    val On  = makeAttr(Some(Console.REVERSED),   1 << 2)
    val Off = makeAttr(None,                     0 << 2)
    val all = Seq(On, Off)
  }

  val hardOffMask = Bold.mask | Underlined.mask | Reversed.mask
  val categories = List(Color, Back, Bold, Underlined, Reversed)

  object Str {
    @sharable lazy val ansiRegex = "\u001B\\[[;\\d]*m".r

    def parse(raw: CharSequence): Str = {
      val chars        = new Array[Char](raw.length)
      val colors       = new Array[Short](raw.length)
      var currentIndex = 0
      var currentColor = 0.toShort

      val matches = ansiRegex.findAllMatchIn(raw)
      val indices = Seq(0) ++ matches.flatMap { m => Seq(m.start, m.end) } ++ Seq(raw.length)

      for {
        Seq(start, end) <- indices.sliding(2).toSeq
        if start != end
      } {
        val frag = raw.subSequence(start, end).toString
        if (frag.charAt(0) == '\u001b' && Attr.ParseMap.contains(frag)) {
          currentColor = Attr.ParseMap(frag).transform(currentColor)
        } else {
          var i = 0
          while(i < frag.length){
            chars(currentIndex) = frag(i)
            colors(currentIndex) = currentColor
            i += 1
            currentIndex += 1
          }
        }
      }

      Str(chars.take(currentIndex), colors.take(currentIndex))
    }
  }

  /**
    * An [[Ansi.Str]]'s `color`s array is filled with shorts, each representing
    * the ANSI state of one character encoded in its bits. Each [[Attr]] belongs
    * to a [[Category]] that occupies a range of bits within each short:
    *
    * 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
    *  |-----------|  |--------|  |--------|  |  |  |bold
    *              |           |           |  |  |reversed
    *              |           |           |  |underlined
    *              |           |           |foreground-color
    *              |           |background-color
    *              |unused
    *
    *
    * The `0000 0000 0000 0000` short corresponds to plain text with no decoration
    *
    */
  type State = Short

  /**
    * Encapsulates a string with associated ANSI colors and text decorations.
    *
    * Contains some basic string methods, as well as some ansi methods to e.g.
    * apply particular colors or other decorations to particular sections of
    * the [[Ansi.Str]]. [[render]] flattens it out into a `java.lang.String`
    * with all the colors present as ANSI escapes.
    *
    */
  case class Str private(chars: Array[Char], colors: Array[State]) {
    require(chars.length == colors.length)

    def ++(other: Str) = Str(chars ++ other.chars, colors ++ other.colors)
    def splitAt(index: Int) = {
      val (leftChars, rightChars) = chars.splitAt(index)
      val (leftColors, rightColors) = colors.splitAt(index)
      (new Str(leftChars, leftColors), new Str(rightChars, rightColors))
    }

    def length = chars.length
    override def toString = render

    def plainText = new String(chars.toArray)
    def render = {
      // Pre-size StringBuilder with approximate size (ansi colors tend
      // to be about 5 chars long) to avoid re-allocations during growth
      val output = new StringBuilder(chars.length + colors.length * 5)


      var currentState = 0.toShort
      /**
        * Emit the ansi escapes necessary to transition
        * between two states, if necessary.
        */
      def emitDiff(nextState: Short) = if (currentState != nextState){
        // Any of these transitions from 1 to 0 within the hardOffMask
        // categories cannot be done with a single ansi escape, and need
        // you to emit a RESET followed by re-building whatever ansi state
        // you previous had from scratch
        if ((currentState & ~nextState & hardOffMask) != 0){
          output.append(Console.RESET)
          currentState = 0
        }

        var categoryIndex = 0
        while(categoryIndex < categories.length){
          val cat = categories(categoryIndex)
          if ((cat.mask & currentState) != (cat.mask & nextState)){
            val attr = cat.bitsMap(nextState & cat.mask)

            if (attr.escape.isDefined) {
              output.append(attr.escape.get)
            }
          }
          categoryIndex += 1
        }
      }

      var i = 0
      while(i < colors.length){
        // Emit ANSI escapes to change colors where necessary
        emitDiff(colors(i))
        currentState = colors(i)
        output.append(chars(i))
        i += 1
      }

      // Cap off the left-hand-side of the rendered string with any ansi escape
      // codes necessary to rest the state to 0
      emitDiff(0)
      output.toString
    }

    /**
      * Overlays the desired color over the specified range of the [[Ansi.Str]].
      */
    def overlay(overlayColor: Attr, start: Int, end: Int) = {
      require(end >= start,
        s"end:$end must be greater than start:$end in AnsiStr#overlay call"
      )
      val colorsOut = new Array[Short](colors.length)
      var i = 0
      while(i < colors.length){
        if (i >= start && i < end) colorsOut(i) = overlayColor.transform(colors(i))
        else colorsOut(i) = colors(i)
        i += 1
      }
      new Str(chars, colorsOut)
    }
  }


}
