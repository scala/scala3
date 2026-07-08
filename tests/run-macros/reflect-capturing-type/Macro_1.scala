//> using options -experimental

import scala.quoted.*

object CCProbe:
  inline def checks(): Unit = ${ checksImpl }

  def checksImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    def check(cond: Boolean, msg: String): Unit =
      if !cond then report.errorAndAbort(s"CC reflect check failed: $msg")

    val defs = Symbol.requiredClass("ccdefs.Defs")
    def infoOf(name: String): TypeRepr =
      val sym = defs.fieldMember(name)
      check(sym.exists, s"member $name exists")
      sym.info

    // Syntactic function-family classification of unreduced alias applications
    val impureSym = infoOf("impure").typeSymbol
    check(defn.isFunctionClass(impureSym), "isFunctionClass(ImpureFunction1)")
    check(defn.isImpureFunctionClass(impureSym), "isImpureFunctionClass(ImpureFunction1)")
    check(!defn.isContextFunctionClass(impureSym), "!isContextFunctionClass(ImpureFunction1)")

    val ctxImpureSym = infoOf("ctxImpure").typeSymbol
    check(defn.isFunctionClass(ctxImpureSym), "isFunctionClass(ImpureContextFunction1)")
    check(defn.isImpureFunctionClass(ctxImpureSym), "isImpureFunctionClass(ImpureContextFunction1)")
    check(defn.isContextFunctionClass(ctxImpureSym), "isContextFunctionClass(ImpureContextFunction1)")

    val pureSym = infoOf("pure").typeSymbol
    check(defn.isFunctionClass(pureSym), "isFunctionClass(Function1)")
    check(!defn.isImpureFunctionClass(pureSym), "!isImpureFunctionClass(Function1)")
    check(!defn.isContextFunctionClass(pureSym), "!isContextFunctionClass(Function1)")

    val ctxPureSym = infoOf("ctxPure").typeSymbol
    check(defn.isContextFunctionClass(ctxPureSym), "isContextFunctionClass(ContextFunction1)")
    check(!defn.isImpureFunctionClass(ctxPureSym), "!isImpureFunctionClass(ContextFunction1)")

    // CapturingType extraction
    infoOf("capped") match
      case CapturingType(parent, refs) =>
        check(parent.typeSymbol == defn.FunctionClass(1), "capped parent is Function1")
        check(refs.map(_.termSymbol.name) == List("a"), s"capped refs {a}, got $refs")
      case t =>
        report.errorAndAbort(s"capped is not a CapturingType: ${t.show(using Printer.TypeReprStructure)}")

    infoOf("universal") match
      case CapturingType(parent, List(ref)) =>
        check(ref.termSymbol == defn.Caps_any, "universal refs {any}")
      case t =>
        report.errorAndAbort(s"universal is not a CapturingType: ${t.show(using Printer.TypeReprStructure)}")

    infoOf("emptySet") match
      case CapturingType(_, refs) => check(refs.isEmpty, s"emptySet has empty refs, got $refs")
      case t =>
        report.errorAndAbort(s"emptySet is not a CapturingType: ${t.show(using Printer.TypeReprStructure)}")

    // The union-type encoding of multi-element capture sets is flattened
    infoOf("multiCap") match
      case CapturingType(_, refs) =>
        check(refs.map(_.termSymbol.name) == List("a", "b"), s"multiCap refs {a, b} flattened, got $refs")
      case t =>
        report.errorAndAbort(s"multiCap is not a CapturingType: ${t.show(using Printer.TypeReprStructure)}")

    // `^` in a function result is the fresh capability
    infoOf("mkFresh") match
      case AppliedType(_, List(CapturingType(_, List(ref)))) =>
        check(ref.termSymbol == defn.Caps_fresh, s"mkFresh result capture is caps.fresh, got $ref")
      case t =>
        report.errorAndAbort(s"mkFresh shape unexpected: ${t.show(using Printer.TypeReprStructure)}")

    // retainedElements extension and interaction with AnnotatedType
    infoOf("capped") match
      case ct: CapturingType =>
        check(ct.retainedElements.map(_.termSymbol.name) == List("a"), "retainedElements of capped")
        check(ct.underlying.typeSymbol == defn.FunctionClass(1), "CapturingType <: AnnotatedType underlying")
      case _ => check(false, "capped matches CapturingType type test")
    check(
      infoOf("pure") match
        case _: CapturingType => false
        case _ => true
      , "pure is not a CapturingType")

    // Read-only and classifier-restricted capabilities
    infoOf("rdCap") match
      case CapturingType(_, List(ReadOnlyCapability(ref))) =>
        check(ref.termSymbol.name == "a", "rdCap ref is a")
      case t =>
        report.errorAndAbort(s"rdCap shape unexpected: ${t.show(using Printer.TypeReprStructure)}")

    infoOf("onlyCap") match
      case CapturingType(_, List(OnlyCapability(ref, classifier))) =>
        check(ref.termSymbol == defn.Caps_any, "onlyCap ref is any")
        check(classifier.name == "Ctrl", s"onlyCap classifier Ctrl, got ${classifier.name}")
      case t =>
        report.errorAndAbort(s"onlyCap shape unexpected: ${t.show(using Printer.TypeReprStructure)}")

    infoOf("exceptCap") match
      case CapturingType(_, List(ExceptCapability(ref, classifier))) =>
        check(ref.termSymbol == defn.Caps_any, "exceptCap ref is any")
        check(classifier.name == "Ctrl", s"exceptCap classifier Ctrl, got ${classifier.name}")
      case t =>
        report.errorAndAbort(s"exceptCap shape unexpected: ${t.show(using Printer.TypeReprStructure)}")

    // By-name parameter with explicit capture set
    defs.methodMember("byname") match
      case bynameSym :: Nil =>
        bynameSym.info match
          case MethodType(_, List(ByNameType(CapturingType(_, refs))), _) =>
            check(refs.map(_.termSymbol.name) == List("a"), "byname param refs {a}")
          case t =>
            report.errorAndAbort(s"byname shape unexpected: ${t.show(using Printer.TypeReprStructure)}")
      case _ => check(false, "byname member exists")

    // RetainingAnnotation over symbol annotations (uses clause)
    val usesRefs = Symbol.requiredClass("ccdefs.HasUsesClause").annotations
      .collectFirst { case RetainingAnnotation(refs) => refs }
    check(usesRefs.exists(_.map(_.termSymbol.name) == List("UsesA")),
      s"uses clause of HasUsesClause, got $usesRefs")

    // Pure class classification
    check(Symbol.requiredClass("ccdefs.PureBase").isPureClass, "PureBase is pure (explicit pure self type)")
    check(Symbol.requiredClass("ccdefs.PureSub").isPureClass, "PureSub is pure (pure base class)")
    check(!Symbol.requiredClass("ccdefs.Impure").isPureClass, "Impure is not pure")
    check(!Symbol.requiredModule("ccdefs.ImpureModule").moduleClass.isPureClass, "modules are not pure via their self type")
    check(defn.StringClass.isPureClass, "String is pure")
    check(Symbol.requiredClass("java.lang.Exception").isPureClass, "Throwable subclasses are pure")
    check(!defs.isPureClass, "Defs is not pure")

    // defn symbols
    check(defn.Caps_CapSet.fullName == "scala.caps.CapSet", "Caps_CapSet")
    check(defn.Caps_Capability.fullName == "scala.caps.Capability", "Caps_Capability")
    check(defn.Caps_fresh.name == "fresh", "Caps_fresh")
    check(defn.ConsumeAnnot == Symbol.requiredClass("scala.caps.internal.consume"), "ConsumeAnnot")

    // Construction round-trips (unapply flattens, so compare decomposed forms)
    CapturingType(TypeRepr.of[String], List(defn.Caps_any.termRef)) match
      case CapturingType(parent, List(ref)) =>
        check(parent =:= TypeRepr.of[String] && ref.termSymbol == defn.Caps_any, "CapturingType round-trip")
      case _ => check(false, "CapturingType round-trip shape")
    CapturingType(TypeRepr.of[String], Nil) match
      case CapturingType(_, refs) => check(refs.isEmpty, "empty CapturingType round-trip")
    check(
      ReadOnlyCapability.unapply(ReadOnlyCapability(defn.Caps_any.termRef))
        .exists(_.termSymbol == defn.Caps_any),
      "ReadOnlyCapability round-trip")
    check(
      OnlyCapability.unapply(OnlyCapability(defn.Caps_any.termRef, Symbol.requiredClass("ccdefs.Ctrl")))
        .exists((ref, cls) => ref.termSymbol == defn.Caps_any && cls.name == "Ctrl"),
      "OnlyCapability round-trip")
    check(
      ExceptCapability.unapply(ExceptCapability(defn.Caps_any.termRef, Symbol.requiredClass("ccdefs.Ctrl")))
        .exists((ref, cls) => ref.termSymbol == defn.Caps_any && cls.name == "Ctrl"),
      "ExceptCapability round-trip")

    '{ () }
