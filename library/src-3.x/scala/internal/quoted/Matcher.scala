package scala.internal.quoted

import scala.annotation.internal.sharable

import scala.quoted._
import scala.quoted.matching.Bind
import scala.tasty._

object Matcher {

  private final val debug = false

  /** Pattern matches an the scrutineeExpr aquainsnt the patternExpr and returns a tuple
   *  with the matched holes if successful.
   *
   *  Examples:
   *    - `Matcher.unapply('{ f(0, myInt) })('{ f(0, myInt) }, _)`
   *       will return `Some(())` (where `()` is a tuple of arity 0)
   *    - `Matcher.unapply('{ f(0, myInt) })('{ f(patternHole[Int], patternHole[Int]) }, _)`
   *       will return `Some(Tuple2('{0}, '{ myInt }))`
   *    - `Matcher.unapply('{ f(0, "abc") })('{ f(0, patternHole[Int]) }, _)`
   *       will return `None` due to the missmatch of types in the hole
   *
   *  Holes:
   *    - scala.internal.Quoted.patternHole[T]: hole that matches an expression `x` of type `Expr[U]`
   *                                            if `U <:< T` and returns `x` as part of the match.
   *
   *  @param scrutineeExpr `Expr[_]` on which we are pattern matching
   *  @param patternExpr `Expr[_]` containing the pattern tree
   *  @param reflection instance of the reflection API (implicitly provided by the macro)
   *  @return None if it did not match, `Some(tup)` if it matched where `tup` contains `Expr[Ti]``
   */
  def unapply[Tup <: Tuple](scrutineeExpr: Expr[_])(implicit patternExpr: Expr[_], reflection: Reflection): Option[Tup] = {
    // TODO improve performance
    import reflection.{Bind => BindPattern, _}
    import Matching._

    type Env = Set[(Symbol, Symbol)]

    inline def withEnv[T](env: Env)(body: => given Env => T): T = body given env

    /** Check that all trees match with `mtch` and concatenate the results with && */
    def matchLists[T](l1: List[T], l2: List[T])(mtch: (T, T) => Matching): Matching = (l1, l2) match {
      case (x :: xs, y :: ys) => mtch(x, y) && matchLists(xs, ys)(mtch)
      case (Nil, Nil) => matched
      case _ => notMatched
    }

    /** Check that all trees match with =#= and concatenate the results with && */
    def (scrutinees: List[Tree]) =##= (patterns: List[Tree]) given Env: Matching =
      matchLists(scrutinees, patterns)(_ =#= _)

    /** Check that the trees match and return the contents from the pattern holes.
     *  Return None if the trees do not match otherwise return Some of a tuple containing all the contents in the holes.
     *
     * @param scrutinee The tree beeing matched
     * @param pattern The pattern tree that the scrutinee should match. Contains `patternHole` holes.
     * @param `the[Env]` Set of tuples containing pairs of symbols (s, p) where s defines a symbol in `scrutinee` which corresponds to symbol p in `pattern`.
     * @return `None` if it did not match or `Some(tup: Tuple)` if it matched where `tup` contains the contents of the holes.
     */
    def (scrutinee: Tree) =#= (pattern: Tree) given Env: Matching = {

      /** Check that both are `val` or both are `lazy val` or both are `var` **/
      def checkValFlags(): Boolean = {
        import Flags._
        val sFlags = scrutinee.symbol.flags
        val pFlags = pattern.symbol.flags
        sFlags.is(Lazy) == pFlags.is(Lazy) && sFlags.is(Mutable) == pFlags.is(Mutable)
      }

      def bindingMatch(sym: Symbol) =
        matched(new Bind(sym.name, sym))

      def hasBindTypeAnnotation(tpt: TypeTree): Boolean = tpt match {
        case Annotated(tpt2, Apply(Select(New(TypeIdent("patternBindHole")), "<init>"), Nil)) => true
        case Annotated(tpt2, _) => hasBindTypeAnnotation(tpt2)
        case _ => false
      }

      def hasBindAnnotation(sym: Symbol) =
        sym.annots.exists { case Apply(Select(New(TypeIdent("patternBindHole")),"<init>"),List()) => true; case _ => true }

      /** Normalieze the tree */
      def normalize(tree: Tree): Tree = tree match {
        case Block(Nil, expr) => normalize(expr)
        case Inlined(_, Nil, expr) => normalize(expr)
        case _ => tree
      }

      (normalize(scrutinee), normalize(pattern)) match {

        // Match a scala.internal.Quoted.patternHole typed as a repeated argument and return the scrutinee tree
        case (IsTerm(scrutinee @ Typed(s, tpt1)), Typed(TypeApply(patternHole, tpt :: Nil), tpt2))
            if patternHole.symbol == kernel.Definitions_InternalQuoted_patternHole &&
               s.tpe <:< tpt.tpe &&
               tpt2.tpe.derivesFrom(definitions.RepeatedParamClass) =>
          matched(scrutinee.seal)

        // Match a scala.internal.Quoted.patternHole and return the scrutinee tree
        case (IsTerm(scrutinee), TypeApply(patternHole, tpt :: Nil))
            if patternHole.symbol == kernel.Definitions_InternalQuoted_patternHole &&
               scrutinee.tpe <:< tpt.tpe =>
          matched(scrutinee.seal)

        //
        // Match two equivalent trees
        //

        case (Literal(constant1), Literal(constant2)) if constant1 == constant2 =>
          matched

        case (Typed(expr1, tpt1), Typed(expr2, tpt2)) =>
          expr1 =#= expr2 && tpt1 =#= tpt2

        case (Ident(_), Ident(_)) if scrutinee.symbol == pattern.symbol || the[Env].apply((scrutinee.symbol, pattern.symbol)) =>
          matched

        case (Select(qual1, _), Select(qual2, _)) if scrutinee.symbol == pattern.symbol =>
          qual1 =#= qual2

        case (IsRef(_), IsRef(_)) if scrutinee.symbol == pattern.symbol =>
          matched

        case (Apply(fn1, args1), Apply(fn2, args2)) if fn1.symbol == fn2.symbol =>
          fn1 =#= fn2 && args1 =##= args2

        case (TypeApply(fn1, args1), TypeApply(fn2, args2)) if fn1.symbol == fn2.symbol =>
          fn1 =#= fn2 && args1 =##= args2

        case (Block(stats1, expr1), Block(stats2, expr2)) =>
          withEnv(the[Env] ++ stats1.map(_.symbol).zip(stats2.map(_.symbol))) {
            stats1 =##= stats2 && expr1 =#= expr2
          }

        case (If(cond1, thenp1, elsep1), If(cond2, thenp2, elsep2)) =>
          cond1 =#= cond2 && thenp1 =#= thenp2 && elsep1 =#= elsep2

        case (Assign(lhs1, rhs1), Assign(lhs2, rhs2)) =>
          val lhsMatch =
            if ((lhs1 =#= lhs2).isMatch) matched
            else notMatched
          lhsMatch && rhs1 =#= rhs2

        case (While(cond1, body1), While(cond2, body2)) =>
          cond1 =#= cond2 && body1 =#= body2

        case (NamedArg(name1, expr1), NamedArg(name2, expr2)) if name1 == name2 =>
          expr1 =#= expr2

        case (New(tpt1), New(tpt2)) =>
          tpt1 =#= tpt2

        case (This(_), This(_)) if scrutinee.symbol == pattern.symbol =>
          matched

        case (Super(qual1, mix1), Super(qual2, mix2)) if mix1 == mix2 =>
          qual1 =#= qual2

        case (Repeated(elems1, _), Repeated(elems2, _)) if elems1.size == elems2.size =>
          elems1 =##= elems2

        case (IsTypeTree(scrutinee @ TypeIdent(_)), IsTypeTree(pattern @ TypeIdent(_))) if scrutinee.symbol == pattern.symbol =>
          matched

        case (IsInferred(scrutinee), IsInferred(pattern)) if scrutinee.tpe <:< pattern.tpe =>
          matched

        case (Applied(tycon1, args1), Applied(tycon2, args2)) =>
          tycon1 =#= tycon2 && args1 =##= args2

        case (ValDef(_, tpt1, rhs1), ValDef(_, tpt2, rhs2)) if checkValFlags() =>
          val bindMatch =
            if (hasBindAnnotation(pattern.symbol) || hasBindTypeAnnotation(tpt2)) bindingMatch(scrutinee.symbol)
            else matched
          def rhsEnv = the[Env] + (scrutinee.symbol -> pattern.symbol)
          bindMatch && tpt1 =#= tpt2 && (treeOptMatches(rhs1, rhs2) given rhsEnv)

        case (DefDef(_, typeParams1, paramss1, tpt1, Some(rhs1)), DefDef(_, typeParams2, paramss2, tpt2, Some(rhs2))) =>
          val bindMatch =
            if (hasBindAnnotation(pattern.symbol)) bindingMatch(scrutinee.symbol)
            else matched
          def rhsEnv =
            the[Env] + (scrutinee.symbol -> pattern.symbol) ++
              typeParams1.zip(typeParams2).map((tparam1, tparam2) => tparam1.symbol -> tparam2.symbol) ++
              paramss1.flatten.zip(paramss2.flatten).map((param1, param2) => param1.symbol -> param2.symbol)

          bindMatch &&
          typeParams1 =##= typeParams2 &&
          matchLists(paramss1, paramss2)(_ =##= _) &&
          tpt1 =#= tpt2 &&
          withEnv(rhsEnv)(rhs1 =#= rhs2)

        case (Lambda(_, tpt1), Lambda(_, tpt2)) =>
          // TODO match tpt1 with tpt2?
          matched

        case (Match(scru1, cases1), Match(scru2, cases2)) =>
          scru1 =#= scru2 && matchLists(cases1, cases2)(caseMatches)

        case (Try(body1, cases1, finalizer1), Try(body2, cases2, finalizer2)) =>
          body1 =#= body2 && matchLists(cases1, cases2)(caseMatches) && treeOptMatches(finalizer1, finalizer2)

        // Ignore type annotations
        case (Annotated(tpt, _), _) =>
          tpt =#= pattern
        case (_, Annotated(tpt, _)) =>
          scrutinee =#= tpt

        // No Match
        case _ =>
          if (debug)
            println(
              s""">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                 |Scrutinee
                 |  ${scrutinee.show}
                 |
                 |${scrutinee.showExtractors}
                 |
                 |did not match pattern
                 |  ${pattern.show}
                 |
                 |${pattern.showExtractors}
                 |
                 |
                 |
                 |
                 |""".stripMargin)
          notMatched
      }
    }

    def treeOptMatches(scrutinee: Option[Tree], pattern: Option[Tree]) given Env: Matching = {
      (scrutinee, pattern) match {
        case (Some(x), Some(y)) => x =#= y
        case (None, None) => matched
        case _ => notMatched
      }
    }

    def caseMatches(scrutinee: CaseDef, pattern: CaseDef) given Env: Matching = {
      val (caseEnv, patternMatch) = scrutinee.pattern =%= pattern.pattern
      withEnv(caseEnv) {
        patternMatch &&
        treeOptMatches(scrutinee.guard, pattern.guard) &&
        scrutinee.rhs =#= pattern.rhs
      }
    }

    /** Check that the pattern trees match and return the contents from the pattern holes.
     *  Return a tuple with the new environment containing the bindings defined in this pattern and a matching.
     *  The matching is None if the pattern trees do not match otherwise return Some of a tuple containing all the contents in the holes.
     *
     * @param scrutinee The pattern tree beeing matched
     * @param pattern The pattern tree that the scrutinee should match. Contains `patternHole` holes.
     * @param `the[Env]` Set of tuples containing pairs of symbols (s, p) where s defines a symbol in `scrutinee` which corresponds to symbol p in `pattern`.
     * @return The new environment containing the bindings defined in this pattern tuppled with
     *         `None` if it did not match or `Some(tup: Tuple)` if it matched where `tup` contains the contents of the holes.
     */
    def (scrutinee: Pattern) =%= (pattern: Pattern) given Env: (Env, Matching) = (scrutinee, pattern) match {
      case (Pattern.Value(v1), Pattern.Unapply(TypeApply(Select(patternHole @ Ident("patternHole"), "unapply"), List(tpt)), Nil, Nil))
          if patternHole.symbol.owner.fullName == "scala.runtime.quoted.Matcher$" =>
        (the[Env], matched(v1.seal))

      case (Pattern.Value(v1), Pattern.Value(v2)) =>
        (the[Env], v1 =#= v2)

      case (Pattern.Bind(name1, body1), Pattern.Bind(name2, body2)) =>
        val bindEnv = the[Env] + (scrutinee.symbol -> pattern.symbol)
        (body1 =%= body2) given bindEnv

      case (Pattern.Unapply(fun1, implicits1, patterns1), Pattern.Unapply(fun2, implicits2, patterns2)) =>
        val (patEnv, patternsMatch) = foldPatterns(patterns1, patterns2)
        (patEnv, fun1 =#= fun2 && implicits1 =##= implicits2 && patternsMatch)

      case (Pattern.Alternatives(patterns1), Pattern.Alternatives(patterns2)) =>
        foldPatterns(patterns1, patterns2)

      case (Pattern.TypeTest(tpt1), Pattern.TypeTest(tpt2)) =>
        (the[Env], tpt1 =#= tpt2)

      case (Pattern.WildcardPattern(), Pattern.WildcardPattern()) =>
        (the[Env], matched)

      case _ =>
        if (debug)
          println(
            s""">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
               |Scrutinee
               |  ${scrutinee.show}
               |
               |${scrutinee.showExtractors}
               |
               |did not match pattern
               |  ${pattern.show}
               |
               |${pattern.showExtractors}
               |
               |
               |
               |
               |""".stripMargin)
        (the[Env], notMatched)
    }

    def foldPatterns(patterns1: List[Pattern], patterns2: List[Pattern]) given Env: (Env, Matching) = {
      if (patterns1.size != patterns2.size) (the[Env], notMatched)
      else patterns1.zip(patterns2).foldLeft((the[Env], matched)) { (acc, x) =>
        val (env, res) = (x._1 =%= x._2) given acc._1
        (env, acc._2 && res)
      }
    }

    implicit val env: Env = Set.empty
    (scrutineeExpr.unseal =#= patternExpr.unseal).asOptionOfTuple.asInstanceOf[Option[Tup]]
  }

  /** Result of matching a part of an expression */
  private opaque type Matching = Option[Tuple]

  private object Matching {

    def notMatched: Matching = None
    val matched: Matching = Some(())
    def matched(x: Any): Matching = Some(Tuple1(x))

    def (self: Matching) asOptionOfTuple: Option[Tuple] = self

    /** Concatenates the contents of two sucessful matchings or return a `notMatched` */
    // FIXME inline to avoid alocation of by name closure (see #6395)
    /*inline*/ def (self: Matching) && (that: => Matching): Matching = self match {
      case Some(x) =>
        that match {
          case Some(y) => Some(x ++ y)
          case _ => None
        }
      case _ => None
    }

    /** Is this matching the result of a successful match */
    def (self: Matching) isMatch: Boolean = self.isDefined

    /** Joins the mattchings into a single matching. If any matching is `None` the result is `None`.
     *  Otherwise the result is `Some` of the concatenation of the tupples.
     */
    def foldMatchings(matchings: Matching*): Matching = {
      // TODO improve performance
      matchings.foldLeft[Matching](Some(())) {
        case (Some(acc), Some(holes)) => Some(acc ++ holes)
        case (_, _) => None
      }
    }

  }

}
