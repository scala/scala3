/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package backend.jvm
package opt

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.tools.asm.Type
import scala.tools.asm.tree.MethodNode
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.BackendReporting.{CalleeNotFinal, CannotInlineWarning, NoBytecode, OptimizerWarning, StrictfpMismatch, SynchronizedMethod}
import dotty.tools.backend.jvm.BackendUtils
import dotty.tools.backend.jvm.opt.InlinerHeuristics.*
import PostProcessorFrontendAccess.Lazy
import dotty.tools.backend.jvm.BCodeUtils.{isStrictfpMethod, isSynchronizedMethod}

class InlinerHeuristics(ppa: PostProcessorFrontendAccess, backendUtils: BackendUtils, byteCodeRepository: BCodeRepository, callGraph: CallGraph, ts: CoreBTypes)(using Context) {

  private lazy val inlineSourceMatcher: Lazy[InlineSourceMatcher] = ppa.perRunLazy(new InlineSourceMatcher(ppa.compilerSettings.optInlineFrom))

  def canInlineFromSource(sourceFilePath: Option[String], calleeDeclarationClass: InternalName): Boolean = {
    inlineSourceMatcher.get.allowFromSources && sourceFilePath.isDefined ||
    inlineSourceMatcher.get.allow(calleeDeclarationClass)
  }

  /**
   * Select callsites from the call graph that should be inlined, grouped by the containing method.
   * Cyclic inlining requests are allowed, the inliner will eliminate requests to break cycles.
   */
  def selectCallsitesForInlining: Map[MethodNode, Set[InlineRequest]] = {
    // We should only create inlining requests for callsites being compiled (not for callsites in
    // classes on the classpath). The call graph may contain callsites of classes parsed from the
    // classpath. In order to get only the callsites being compiled, we start at the map of
    // compilingClasses in the byteCodeRepository.
    val compilingMethods = for {
      (classNode, _) <- byteCodeRepository.compilingClasses.get.valuesIterator
      methodNode     <- classNode.methods.iterator.asScala
    } yield methodNode

    compilingMethods.map(methodNode => {
      var requests = Set.empty[InlineRequest]
      callGraph.callsites.get(methodNode).valuesIterator foreach {
        case callsite @ Callsite(_, _, _, Right(Callee(callee, _, _, _, _, _, _, callsiteWarning)), _, _, _, pos, _, _) =>
          inlineRequest(callsite) match {
            case Some(Right(req)) => requests += req

            case Some(Left(w)) =>
              if (w.emitWarning(ppa.compilerSettings)) {
                ppa.backendReporting.optimizerWarning(em"${w.toString}", ppa.backendReporting.siteString(callsite.callsiteClass.internalName, callsite.callsiteMethod.name), callsite.callsitePosition)
              }

            case None =>
              if (callsiteWarning.isDefined && callsiteWarning.get.emitWarning(ppa.compilerSettings)) {
                ppa.backendReporting.optimizerWarning(em"there was a problem determining if method ${callee.name} can be inlined: \n${callsiteWarning.get.toString}", ppa.backendReporting.siteString(callsite.callsiteClass.internalName, callsite.callsiteMethod.name), pos)
              }
          }

        case callsite @ Callsite(ins, _, _, Left(warning), _, _, _, pos, _, _) =>
          if (warning.emitWarning(ppa.compilerSettings)) {
            ppa.backendReporting.optimizerWarning(em"failed to determine if ${ins.name} should be inlined:\n${warning.toString}", ppa.backendReporting.siteString(callsite.callsiteClass.internalName, callsite.callsiteMethod.name), pos)
          }
      }
      (methodNode, requests)
    }).filterNot(_._2.isEmpty).toMap
  }

  val maxSize = 3000
  val mediumSize = 2000
  val smallSize = 1000

  def selectRequestsForMethodSize(method: MethodNode, requests: List[InlineRequest], methodSizes: mutable.Map[MethodNode, Int]): List[InlineRequest] = {
    val byReason = requests.groupBy(_.reason)
    var size = method.instructions.size
    val res = mutable.ListBuffer.empty[InlineRequest]
    def include(kind: InlineReason, limit: Int): Unit = {
      var rs = byReason.getOrElse(kind, Nil)
      while (rs.nonEmpty && size < limit) {
        val r = rs.head
        rs = rs.tail
        val callee = r.callsite.callee.get.callee
        val cSize = methodSizes.getOrElse(callee, callee.instructions.size)
        if (size + cSize < limit) {
          res += r
          size += cSize
        }
      }
    }
    include(AnnotatedInline, maxSize)
    include(SyntheticForwarder, maxSize)
    include(KnownArrayOp, maxSize)
    include(HigherOrderWithLiteral, maxSize)
    include(HigherOrderWithForwardedParam, mediumSize)
    include(RefParam, mediumSize)
    include(BoxingForwarder, mediumSize)
    include(FactoryMethod, mediumSize)
    include(GenericForwarder, smallSize)
    include(TrivialMethod, smallSize)
    methodSizes(method) = size
    res.toList
  }

  /**
   * Check whether an inlining can be performed. This method performs tests that don't change even
   * if the body of the callee is changed by the inliner / optimizer, so it can be used early
   * (when looking at the call graph and collecting inline requests for the program).
   *
   * The tests that inspect the callee's instructions are implemented in method `canInlineBody`,
   * which is queried when performing an inline.
   *
   * @return `Some(message)` if inlining cannot be performed, `None` otherwise
   */
  private def earlyCanInlineCheck(callsite: Callsite): Option[CannotInlineWarning] = {
    import callsite.{callsiteClass, callsiteMethod}
    val Right(callsiteCallee) = callsite.callee: @unchecked
    import callsiteCallee.{callee, calleeDeclarationClass}

    if (isSynchronizedMethod(callee)) {
      // Could be done by locking on the receiver, wrapping the inlined code in a try and unlocking
      // in finally. But it's probably not worth the effort, scala never emits synchronized methods.
      Some(SynchronizedMethod(calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated))
    } else if (isStrictfpMethod(callsiteMethod) != isStrictfpMethod(callee)) {
      Some(StrictfpMismatch(
        calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated,
        callsiteClass.internalName, callsiteMethod.name, callsiteMethod.desc))
    } else if (callee.instructions.size == 0) {
      Some(NoBytecode(calleeDeclarationClass.internalName, callee.name, callee.desc, callsite.isInlineAnnotated))
    } else
      None
  }

  /**
   * Returns the inline request for a callsite if the callsite should be inlined according to the
   * current heuristics (`-Yopt-inline-heuristics`).
   *
   * @return `None` if this callsite should not be inlined according to the active heuristic
   *         `Some(Left)` if the callsite should be inlined according to the heuristic, but cannot
   *           be inlined according to an early, incomplete check (see earlyCanInlineCheck)
   *         `Some(Right)` if the callsite should be inlined (it's still possible that the callsite
   *           cannot be inlined in the end, for example if it contains instructions that would
   *           cause an IllegalAccessError in the new class; this is checked in the inliner)
   */
  def inlineRequest(callsite: Callsite): Option[Either[OptimizerWarning, InlineRequest]] = {
    def requestIfCanInline(callsite: Callsite, reason: InlineReason): Option[Either[OptimizerWarning, InlineRequest]] = {
      val callee = callsite.callee.get
      val canInlineFromSource0 = canInlineFromSource(callee.sourceFilePath, callee.calleeDeclarationClass.internalName)
      if (!(callee.isStaticallyResolved && canInlineFromSource0 && !callee.isAbstract && !callee.isSpecialMethod)) {
        if (callsite.isInlineAnnotated && canInlineFromSource0) {
          // By default, we only emit inliner warnings for methods annotated @inline. However, we don't
          // want to be unnecessarily noisy with optimizer warnings: for example, the inliner heuristic
          // would attempt to inline `Function1.apply$sp$II`, as it's higher-order (the receiver is
          // a function), and it's concrete (forwards to `apply`). But because it's non-final, it cannot
          // be inlined. So we only create warnings here for methods annotated @inline.
          Some(Left(CalleeNotFinal(
            callee.calleeDeclarationClass.internalName,
            callee.callee.name,
            callee.callee.desc,
            callsite.isInlineAnnotated)))
        } else None
      } else earlyCanInlineCheck(callsite) match {
        case Some(w) =>
          Some(Left(w))
        case None =>
          Some(Right(InlineRequest(callsite, reason, ppa.compilerSettings.optLogInline.isEmpty, ppa.compilerSettings.optInlineHeuristics == "everything")))
      }
    }

    // don't inline into synthetic forwarders (anonfun-adapted methods, bridges, etc.). the heuristics
    // will instead inline such methods at callsite. however, *do* inline into user-written forwarders
    // or aliases, because otherwise it's too confusing for users looking at generated code, they will
    // write a small test method and think the inliner doesn't work correctly.
    val isGeneratedForwarder =
      BCodeUtils.isSyntheticMethod(callsite.callsiteMethod) && backendUtils.looksLikeForwarderOrFactoryOrTrivial(callsite.callsiteMethod, callsite.callsiteClass.internalName, allowPrivateCalls = true) > 0 ||
        BackendUtils.isMixinForwarder(callsite.callsiteMethod, callsite.callsiteClass) // seems mixin forwarders are not synthetic...

    if (isGeneratedForwarder) None
    else {
      val callee = callsite.callee.get
      ppa.compilerSettings.optInlineHeuristics match {
        case "everything" =>
          requestIfCanInline(callsite, AnnotatedInline)

        case "at-inline-annotated" =>
          if (callsite.isInlineAnnotated && !callsite.isNoInlineAnnotated) requestIfCanInline(callsite, AnnotatedInline)
          else None

        case "default" =>
          def shouldInlineAnnotated = if (callsite.isInlineAnnotated) Some(AnnotatedInline) else None

          def shouldInlineHO = Option {
            if (callee.samParamTypes.isEmpty) null
            else {
              val samArgs = callee.samParamTypes flatMap {
                case (index, _) => Option.option2Iterable(callsite.argInfos.get(index))
              }
              if (samArgs.isEmpty) null
              else if (samArgs.exists(_ == FunctionLiteral)) HigherOrderWithLiteral
              else HigherOrderWithForwardedParam
            }
          }

          def shouldInlineRefParam =
            if (Type.getArgumentTypes(callee.callee.desc).exists(tp => ts.srRefCreateMethods.contains(tp.getInternalName))) Some(RefParam)
            else None

          def shouldInlineArrayOp =
            if (BackendUtils.isRuntimeArrayLoadOrUpdate(callsite.callsiteInstruction) && callsite.argInfos.get(1).contains(StaticallyKnownArray)) Some(KnownArrayOp)
            else None

          def shouldInlineForwarder = Option {
            // trait super accessors are excluded here because they contain an `invokespecial` of the default method in the trait.
            // this instruction would have different semantics if inlined into some other class.
            // we *do* inline trait super accessors if selected by a different heuristic. in this case, the `invokespecial` is then
            // inlined in turn (chosen by the same heuristic), or the code is rolled back. but we don't inline them just because
            // they are forwarders.
            val isTraitSuperAccessor = BackendUtils.isTraitSuperAccessor(callee.callee, callee.calleeDeclarationClass)
            if (isTraitSuperAccessor) {
              // inline static trait super accessors if the corresponding trait method is a forwarder or trivial (scala-dev#618)
              {
                val css = callGraph.callsites.get(callee.callee)
                if (css.sizeIs == 1) css.head._2 else null
              } match {
                case null => null
                case traitMethodCallsite =>
                  val tmCallee = traitMethodCallsite.callee.get
                  val traitMethodForwarderKind = backendUtils.looksLikeForwarderOrFactoryOrTrivial(
                    tmCallee.callee, tmCallee.calleeDeclarationClass.internalName, allowPrivateCalls = false)
                  if (traitMethodForwarderKind > 0) GenericForwarder
                  else null
              }
            }
            else {
              val forwarderKind = backendUtils.looksLikeForwarderOrFactoryOrTrivial(callee.callee, callee.calleeDeclarationClass.internalName, allowPrivateCalls = false)
              if (forwarderKind < 0)
                null
              else if (BCodeUtils.isSyntheticMethod(callee.callee) || BackendUtils.isMixinForwarder(callee.callee, callee.calleeDeclarationClass))
                SyntheticForwarder
              else forwarderKind match {
                case 1 => TrivialMethod
                case 2 => FactoryMethod
                case 3 => BoxingForwarder
                case 4 => GenericForwarder
              }
            }
          }

          if (callsite.isNoInlineAnnotated) None
          else {
            val reason = shouldInlineAnnotated.orElse(shouldInlineHO).orElse(shouldInlineRefParam).orElse(shouldInlineArrayOp).orElse(shouldInlineForwarder)
            reason.flatMap(r => requestIfCanInline(callsite, r))
          }
      }
    }
  }
}

object InlinerHeuristics {
  sealed trait InlineReason
  case object AnnotatedInline extends InlineReason
  case object SyntheticForwarder extends InlineReason
  case object TrivialMethod extends InlineReason
  case object FactoryMethod extends InlineReason
  case object BoxingForwarder extends InlineReason
  case object GenericForwarder extends InlineReason
  case object RefParam extends InlineReason
  case object KnownArrayOp extends InlineReason
  case object HigherOrderWithLiteral extends InlineReason
  case object HigherOrderWithForwardedParam extends InlineReason

  class InlineSourceMatcher(inlineFromSetting: List[String]) {
    // `terminal` is true if all remaining entries are of the same negation as this one
    case class Entry(pattern: Pattern, negated: Boolean, terminal: Boolean) {
      def matches(internalName: InternalName): Boolean = pattern.matcher(internalName).matches()
    }
    private val patternStrings = inlineFromSetting.filterNot(_.isEmpty)
    private val startAllow: Boolean = patternStrings.headOption.contains("**")
    private var _allowFromSources: Boolean = false

    val entries: List[Entry] = parse()

    def allowFromSources: Boolean = _allowFromSources

    def allow(internalName: InternalName): Boolean = {
      var answer = startAllow
      @tailrec def check(es: List[Entry]): Boolean = es match {
        case e :: rest =>
          if (answer && e.negated && e.matches(internalName))
            answer = false
          else if (!answer && !e.negated && e.matches(internalName))
            answer = true

          if (e.terminal && answer != e.negated) answer
          else check(rest)

        case _ =>
          answer
      }
      check(entries)
    }

    private def parse(): List[Entry] = {
      var result = List.empty[Entry]

      val patternsRevIterator = {
        val it = patternStrings.reverseIterator
        if (startAllow) it.take(patternStrings.length - 1) else it
      }
      for (p <- patternsRevIterator) {
        if (p == "<sources>") _allowFromSources = true
        else {
          val len = p.length
          var index = 0

          def current = if (index < len) p.charAt(index) else 0.toChar

          def next(): Unit = index += 1

          val negated = current == '!'
          if (negated) next()

          val regex = new java.lang.StringBuilder

          while (index < len) {
            if (current == '*') {
              next()
              if (current == '*') {
                next()
                val starStarDot = current == '.'
                if (starStarDot) {
                  next()
                  // special case: "a.**.C" matches "a.C", and "**.C" matches "C"
                  val i = index - 4
                  val allowEmpty = i < 0 || (i == 0 && p.charAt(i) == '!') || p.charAt(i) == '.'
                  if (allowEmpty) regex.append("(?:.*/|)")
                  else regex.append(".*/")
                } else
                  regex.append(".*")
              } else {
                regex.append("[^/]*")
              }
            } else if (current == '.') {
              next()
              regex.append('/')
            } else {
              val start = index
              var needEscape = false
              while (index < len && current != '.' && current != '*') {
                needEscape = needEscape || "\\.[]{}()*+-?^$|".indexOf(current) != -1
                next()
              }
              if (needEscape) regex.append("\\Q")
              regex.append(p, start, index)
              if (needEscape) regex.append("\\E")
            }
          }

          val isTerminal = result.isEmpty || result.head.terminal && result.head.negated == negated
          result ::= Entry(Pattern.compile(regex.toString), negated, isTerminal)
        }
      }
      result
    }
  }
}

final case class InlineRequest(callsite: Callsite, reason: InlineReason, logAnyInline: Boolean, inlineEverything: Boolean) {
  // non-null if `-Yopt-log-inline` is active, it explains why the callsite was selected for inlining
  def logText: String | Null =
    if (logAnyInline) null
    else if (inlineEverything) "-Yopt-inline-heuristics:everything is enabled"
    else {
      val callee = callsite.callee.get
      reason match {
        case AnnotatedInline =>
          val what = if (callee.annotatedInline) "callee" else "callsite"
          s"the $what is annotated `@inline`"
        case HigherOrderWithLiteral | HigherOrderWithForwardedParam =>
          val paramNames = Option(callee.callee.parameters).map(_.asScala.map(_.name).toVector)

          def param(i: Int) = {
            def syn = s"<param $i>"

            paramNames.fold(syn)(v => v.applyOrElse(i, (_: Int) => syn))
          }

          def samInfo(i: Int, sam: String, arg: String) = s"the argument for parameter (${param(i)}: $sam) is a $arg"

          val argInfos = for ((i, sam) <- callee.samParamTypes; info <- callsite.argInfos.get(i).iterator) yield {
            val argKind = info match {
              case FunctionLiteral => "function literal"
              case ForwardedParam(_) => "parameter of the callsite method"
              case StaticallyKnownArray => "" // should not happen, just included to avoid potential crash
            }
            samInfo(i, sam.internalName.split('/').last, argKind)
          }
          s"the callee is a higher-order method, ${argInfos.mkString(", ")}"
        case SyntheticForwarder =>
          "the callee is a synthetic forwarder method"
        case TrivialMethod =>
          "the callee is a small trivial method"
        case FactoryMethod =>
          "the callee is a factory method"
        case BoxingForwarder =>
          "the callee is a forwarder method with boxing adaptation"
        case GenericForwarder =>
          "the callee is a forwarder or alias method"
        case RefParam =>
          "the callee has a Ref type parameter"
        case KnownArrayOp =>
          "ScalaRuntime.array_apply and array_update are inlined if the array has a statically known type"
      }
    }
}