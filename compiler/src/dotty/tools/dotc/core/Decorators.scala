package dotty.tools
package dotc
package core

import Contexts.*, Names.*, Phases.*, Symbols.*, Types.*
import printing.{ Printer, Showable }, printing.Formatting.*, printing.Texts.*
import transform.MegaPhase
import reporting.{Message, NoExplanation}

/** This object provides useful extension methods for types defined elsewhere */
object Decorators {

  // Must match VectorStatics.WIDTH: `Vector.fromArray1Unsafe` accepts only the
  // compact Vector1 layout.
  inline val MaxVector1Length = 32

  /** Build a `Vector` of length `len` by filling each slot with `elem(i)`,
   *  taking the array-backed fast path for short (`<= MaxVector1Length`) results.
   *  A faster `Vector.tabulate`: `elem` is inlined and the single-node case
   *  avoids the generic builder. Backs `zipMap`, `mapconserve` and the
   *  index-built parameter vectors in `Types`.
   */
  inline def fillVector[V](len: Int)(inline elem: Int => V): Vector[V] =
    if len == 0 then Vector.empty
    else if len <= MaxVector1Length then
      val elems = new Array[AnyRef](len)
      var i = 0
      while i < len do
        elems(i) = elem(i).asInstanceOf[AnyRef]
        i += 1
      Vector.fromArray1Unsafe(elems).asInstanceOf[Vector[V]]
    else
      val b = Vector.newBuilder[V]
      b.sizeHint(len)
      var i = 0
      while i < len do
        b += elem(i)
        i += 1
      b.result()

  /** Extension methods for toType/TermName methods on PreNames.
   */
  extension (pn: PreName)
    def toTermName: TermName = pn match
      case s: String => termName(s)
      case n: Name => n.toTermName
    def toTypeName: TypeName = pn match
      case s: String => typeName(s)
      case n: Name => n.toTypeName

  extension (s: String)
    def splitWhere(f: Char => Boolean, doDropIndex: Boolean): Option[(String, String)] =
      def splitAt(idx: Int, doDropIndex: Boolean): Option[(String, String)] =
        if (idx == -1) None
        else Some((s.take(idx), s.drop(if (doDropIndex) idx + 1 else idx)))
      splitAt(s.indexWhere(f), doDropIndex)

    /** Create a term name from a string slice, using a common buffer.
     *  This avoids some allocation relative to `termName(s)`
     */
    def sliceToTermName(start: Int, end: Int)(using Context): SimpleName =
      val len = end - start
      val chars = ctx.base.sharedCharArray(len)
      s.getChars(start, end, chars, 0)
      termName(chars, 0, len)

    def sliceToTypeName(start: Int, end: Int)(using Context): TypeName =
      sliceToTermName(start, end).toTypeName

    def concat(name: Name)(using Context): SimpleName = name match
      case name: SimpleName =>
        val len = s.length + name.length
        var chars = ctx.base.sharedCharArray(len)
        s.getChars(0, s.length, chars, 0)
        if name.length != 0 then name.getChars(0, name.length, chars, s.length)
        termName(chars, 0, len)
      case name: TypeName => s.concat(name.toTermName)
      case _ => termName(s.concat(name.toString))

    def indented(width: Int): String =
      val padding = " " * width
      padding + s.replace("\n", "\n" + padding)

    def join(sep: String, other: String) =
      if s.isEmpty then other
      else if other.isEmpty then s
      else s + sep + other
  end extension

  /** Convert lazy string to message. To be with caution, since no message-defined
   *  formatting will be done on the string.
   */
  extension (str: => String)
    def toMessage: Message = NoExplanation(str)(using NoContext)

  /** Implements a findSymbol method on iterators of Symbols that
   *  works like find but avoids Option, replacing None with NoSymbol.
   */
  extension (it: Iterator[Symbol])
    final def findSymbol(p: Symbol => Boolean): Symbol = {
      while (it.hasNext) {
        val sym = it.next()
        if (p(sym)) return sym
      }
      NoSymbol
    }

  extension (tp: Type)
    /** Replace synthetic parameter names (`x$0`, `x$1`, ...) of any method
     *  type group in `tp` (recursing through curried `MethodType`s and
     *  through `PolyType` result types) with dollar-free names (`x0`,
     *  `x1`, ...). Lets a printed signature be reused as a valid Scala
     *  identifier - e.g. in stub implementations or "method is not
     *  defined" diagnostics.
     */
    def withCleanParamNames(using Context): Type = tp match
      case mt: MethodType if mt.allParamNamesSynthetic =>
        val newNames = mt.paramNames.zipWithIndex.map((_, i) => termName("x" + i))
        mt.derivedLambdaType(newNames, mt.paramInfos, mt.resType.withCleanParamNames)
      case mt: MethodType =>
        mt.derivedLambdaType(mt.paramNames, mt.paramInfos, mt.resType.withCleanParamNames)
      case pt: PolyType =>
        pt.derivedLambdaType(pt.paramNames, pt.paramInfos, pt.resType.withCleanParamNames)
      case _ => tp

  /** Implements filterConserve, zipWithConserve methods
   *  on vectors that avoid duplication of vector nodes where feasible.
   */
  extension [T](xs: Vector[T])

    /** Like `xs.map(f)` but returns `xs` itself - instead of a copy - if `f` maps
     *  every element to itself. Declared `inline` so each call site gets its own
     *  specialized loop: `f` is applied monomorphically instead of through a
     *  megamorphic `Function1`, which matters on the hottest compiler paths
     *  (type-argument and parameter-info mapping).
     */
    inline def mapconserve[U](inline f: T => U): Vector[U] = {
      val len = xs.length
      // Scan for the first element `f` changes, applying `f` once per element.
      var changed = -1
      var changedElem: U = null.asInstanceOf[U]
      var i = 0
      while changed < 0 && i < len do
        val x = xs(i)
        val y = f(x)
        if y.asInstanceOf[AnyRef] eq x.asInstanceOf[AnyRef] then i += 1
        else
          changed = i
          changedElem = y
      if changed < 0 then xs.asInstanceOf[Vector[U]]
      else
        // Reuse the unchanged prefix and the already-computed element; map the
        // rest. `fillVector` takes the array-backed fast path for short results.
        fillVector(len): j =>
          if j < changed then xs(j).asInstanceOf[U]
          else if j == changed then changedElem
          else f(xs(j))
    }

    inline def mapConserve[U](inline f: T => U): Vector[U] = mapconserve(f)

    /** Like `xs filter p` but returns vector `xs` itself  - instead of a copy -
     *  if `p` is true for all elements.
     */
    def filterConserve(p: T => Boolean): Vector[T] =
      val len = xs.length
      var i = 0
      while i < len do
        val x = xs(i)
        if !p(x) then
          val b = Vector.newBuilder[T]
          b.sizeHint(len - 1)
          var j = 0
          while j < i do
            b += xs(j)
            j += 1
          i += 1
          while i < len do
            val x = xs(i)
            if p(x) then b += x
            i += 1
          return b.result()
        i += 1
      xs
    end filterConserve

    /** Like `xs.lazyZip(ys).map(f)`, but returns vector `xs` itself
     *  - instead of a copy - if function `f` maps all elements of
     *  `xs` to themselves. Also, it is required that `ys` is at least
     *  as long as `xs`.
     */
    def zipWithConserve[U, V <: T](ys: Vector[U])(f: (T, U) => V): Vector[V] =
      val len = math.min(xs.length, ys.length)
      var i = 0
      while i < len do
        val x = xs(i)
        val y = f(x, ys(i))
        if !(y.asInstanceOf[AnyRef] eq x.asInstanceOf[AnyRef]) then
          val b = Vector.newBuilder[V]
          b.sizeHint(len)
          var j = 0
          while j < i do
            b += xs(j).asInstanceOf[V]
            j += 1
          b += y
          i += 1
          while i < len do
            b += f(xs(i), ys(i))
            i += 1
          return b.result()
        i += 1
      if len == xs.length then xs.asInstanceOf[Vector[V]]
      else xs.take(len).asInstanceOf[Vector[V]]

    /** Like `xs.lazyZip(ys).map(f)`, but avoids the generic lazyZip builder path. */
    def zipMap[U, V](ys: Vector[U])(f: (T, U) => V): Vector[V] =
      val len = math.min(xs.length, ys.length)
      fillVector(len)(i => f(xs(i), ys(i)))

    /** Like `xs.lazyZip(ys).lazyZip(zs).map(f)`, but avoids the generic lazyZip builder path. */
    def zipMap[U, V, W](ys: Vector[U], zs: Vector[V])(f: (T, U, V) => W): Vector[W] =
      val len = math.min(math.min(xs.length, ys.length), zs.length)
      fillVector(len)(i => f(xs(i), ys(i), zs(i)))

    /** Like `xs.lazyZip(ys).foldLeft(z)(op)`: fold `op` over both vectors in
     *  lockstep, stopping at the shorter one. Inlined so `op` is applied
     *  monomorphically and no zip iterator is allocated (used on the hot
     *  `TypeAccumulator` path over type arguments and their parameters).
     */
    inline def zipFoldLeft[U, B](ys: Vector[U])(z: B)(inline op: (B, T, U) => B): B =
      val len = math.min(xs.length, ys.length)
      var acc = z
      var i = 0
      while i < len do
        acc = op(acc, xs(i), ys(i))
        i += 1
      acc

    /** Like `xs.lazyZip(xs.indices).map(f)`, but returns vector `xs` itself
     *  - instead of a copy - if function `f` maps all elements of
     *  `xs` to themselves.
     */
    def mapWithIndexConserve[U <: T](f: (T, Int) => U): Vector[U] =
      val len = xs.length
      var i = 0
      while i < len do
        val x = xs(i)
        val y = f(x, i)
        if !(y.asInstanceOf[AnyRef] eq x.asInstanceOf[AnyRef]) then
          val b = Vector.newBuilder[U]
          b.sizeHint(len)
          var j = 0
          while j < i do
            b += xs(j).asInstanceOf[U]
            j += 1
          b += y
          i += 1
          while i < len do
            b += f(xs(i), i)
            i += 1
          return b.result()
        i += 1
      xs.asInstanceOf[Vector[U]]
    end mapWithIndexConserve

    /** True if two vectors have the same length.
     */
    final def hasSameLengthAs[U](ys: Vector[U]): Boolean =
      xs.length == ys.length

    final def eqElements(ys: Vector[AnyRef]): Boolean =
      if xs.length != ys.length then false
      else
        var i = 0
        while i < xs.length do
          if !xs(i).asInstanceOf[AnyRef].eq(ys(i)) then return false
          i += 1
        true

    /** Union on vectors seen as sets */
    def setUnion (ys: Vector[T]): Vector[T] = xs ++ ys.filterNot(xs contains _)

    /** Reduce left with `op` as long as list `xs` is not longer than `seqLimit`.
     *  Otherwise, split list in two half, reduce each, and combine with `op`.
     */
    def reduceBalanced(op: (T, T) => T, seqLimit: Int = 100): T =
      val len = xs.length
      if len > seqLimit then
        val (leading, trailing) = xs.splitAt(len / 2)
        op(leading.reduceBalanced(op, seqLimit), trailing.reduceBalanced(op, seqLimit))
      else
        xs.reduceLeft(op)

  extension [T, U](xss: Vector[Vector[T]])
    def nestedMap(f: T => U): Vector[Vector[U]] =
      xss.map(_.map(f))
    def nestedMapConserve(f: T => U): Vector[Vector[U]] =
      xss.mapconserve(_.mapconserve(f))
    def nestedZipWithConserve(yss: Vector[Vector[U]])(f: (T, U) => T): Vector[Vector[T]] =
      xss.zipWithConserve(yss)((xs, ys) => xs.zipWithConserve(ys)(f))
    def nestedExists(p: T => Boolean): Boolean =
      xss.exists(_.exists(p))
    def nestedFind(p: T => Boolean): Option[T] =
      xss.iterator.flatMap(_.iterator).find(p)
  end extension

  extension (text: Text)
    def show(using Context): String = text.mkString(ctx.settings.pageWidth.value)

  /** Test whether a list of strings representing phases contains
   *  a given phase. See [[config.CompilerCommand#explainAdvanced]] for the
   *  exact meaning of "contains" here.
   */
   extension (names: Vector[String])
    def containsPhase(phase: Phase): Boolean =
      names.nonEmpty && {
        phase match {
          case phase: MegaPhase => phase.miniPhases.exists(x => names.containsPhase(x))
          case _ =>
            names exists { name =>
              name == "all" || {
                val strippedName = name.stripSuffix("+")
                val logNextPhase = name != strippedName
                phase.phaseName.startsWith(strippedName) ||
                  (logNextPhase && phase.prev.phaseName.startsWith(strippedName))
              }
            }
        }
      }

  extension [T](x: T)
    def showing[U](
        op: WrappedResult[U] ?=> String,
        printer: config.Printers.Printer = config.Printers.default)(using c: Conversion[T, U] | Null = null): T = {
      // either the use of `$result` was driven by the expected type of `Shown`
      // which led to the summoning of `Conversion[T, Shown]` (which we'll invoke)
      // or no such conversion was found so we'll consume the result as it is instead
      val obj = if c == null then x.asInstanceOf[U] else c(x)
      printer.println(op(using WrappedResult(obj)))
      x
    }

    /** Instead of `toString` call `show` on `Showable` values, falling back to `toString` if an exception is raised. */
    def tryToShow(using Context): String = x match
      case x: Showable =>
        try x.show
        catch
          case ex: CyclicReference => "... (caught cyclic reference) ..."
          case ex: Exception
          if !ctx.settings.YshowPrintErrors.value =>
            s"... (cannot display due to ${ex.className} ${ex.getMessage}) ..."
      case _ => String.valueOf(x)

    /** Returns the simple class name of `x`. */
    def className: String = if x == null then "<null>" else x.getClass.getSimpleName

  extension [T](x: T)
    def assertingErrorsReported(using Context): T = {
      assert(ctx.reporter.errorsReported)
      x
    }
    def assertingErrorsReported(msg: Message)(using Context): T = {
      assert(ctx.reporter.errorsReported, msg)
      x
    }

  extension [T <: AnyRef](xs: Vector[T])
    def derivedCons(x1: T, xs1: Vector[T]) =
      def sameTail =
        val len = xs.length
        if xs1.length != len - 1 then false
        else
          var i = 1
          while i < len && xs(i) == xs1(i - 1) do
            i += 1
          i == len
      if xs.nonEmpty && (xs.head eq x1) && sameTail then xs else x1 +: xs1

  extension (sc: StringContext)

    /** General purpose string formatting */
    def i(args: Shown*)(using Context): String =
      new StringFormatter(sc).assemble(args)

    /** Interpolator yielding an error message, which undergoes
     *  the formatting defined in Message.
     */
    def em(args: Shown*)(using Context): NoExplanation =
      NoExplanation(i(args*))

  extension [T <: AnyRef](arr: Array[T])
    def binarySearch(x: T | Null): Int = java.util.Arrays.binarySearch(arr.asInstanceOf[Array[Object | Null]], x)

}
