package dotty.tools.debug

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.NameKinds.QualifiedInfo
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.TypeErasure.ErasedValueType
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.transform.ValueClasses

/**
  * This phase transforms every reflectEval call to an actual method call that performs reflection.
  * Specifically it does:
  *   - encode symbols to Java
  *   - box and unbox value classes where necessary
  *   - box and unbox captured variables where necessary
  *   - evaluate by-name params where necessary
  *   - resolve captured variables and check they are available (they may not be captured at runtime)
  * 
  * Before:
  *   this.reflectEval(a, "ReflectEvalStrategy.MethodCall(m)", args)
  * 
  * After:
  *   this.callMethod(a, "example.A", "m", ["ArgType1", "ArgType2"], "ResType", args)
  *
  */ 
private class ResolveReflectEval(config: ExpressionCompilerConfig, expressionStore: ExpressionStore) extends MiniPhase:
  private val reflectEvalName = termName("reflectEval")
  private val elemName = termName("elem")
  override def phaseName: String = ResolveReflectEval.name

  override def transformTypeDef(tree: TypeDef)(using Context): Tree =
    ExpressionTransformer.transform(tree)

  object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: DefDef if tree.symbol == config.evaluateMethod =>
          // unbox the result of the `evaluate` method if it is a value class
          val gen = new Gen(
            Apply(
              Select(This(config.expressionClass), reflectEvalName),
              List(nullLiteral, nullLiteral, nullLiteral)
            )
          )
          val rhs = gen.unboxIfValueClass(expressionStore.symbol.nn, transform(tree.rhs))
          cpy.DefDef(tree)(rhs = rhs)

        case reflectEval: Apply if isReflectEval(reflectEval.fun.symbol) =>
          val qualifier :: _ :: argsTree :: Nil = reflectEval.args.map(transform): @unchecked
          val args = argsTree.asInstanceOf[JavaSeqLiteral].elems
          val gen = new Gen(reflectEval)
          tree.attachment(ReflectEvalStrategy) match
            case ReflectEvalStrategy.This(cls) => gen.getThisObject
            case ReflectEvalStrategy.LocalOuter(cls) => gen.getLocalValue("$outer")
            case ReflectEvalStrategy.Outer(outerCls) => gen.getOuter(qualifier, outerCls)
            
            case ReflectEvalStrategy.LocalValue(variable, isByName) =>
              val variableName = JavaEncoding.encode(variable.name)
              val rawLocalValue = gen.getLocalValue(variableName)
              val localValue = if isByName then gen.evaluateByName(rawLocalValue) else rawLocalValue
              val derefLocalValue = gen.derefCapturedVar(localValue, variable)
              gen.boxIfValueClass(variable, derefLocalValue)
            
            case ReflectEvalStrategy.LocalValueAssign(variable) =>
              val value = gen.unboxIfValueClass(variable, args.head)
              val typeSymbol = variable.info.typeSymbol.asType
              val variableName = JavaEncoding.encode(variable.name)
              JavaEncoding.encode(typeSymbol) match
                case s"scala.runtime.${_}Ref" =>
                  val elemField = typeSymbol.info.decl(elemName).symbol
                  gen.setField(tree)(
                    gen.getLocalValue(variableName),
                    elemField.asTerm,
                    value
                  )
                case _ => gen.setLocalValue(variableName, value)
            
            case ReflectEvalStrategy.ClassCapture(variable, cls, isByName) =>
              val rawCapture = gen
                .getClassCapture(tree)(qualifier, variable.name, cls)
                .getOrElse {
                  report.error(s"No capture found for $variable in $cls", tree.srcPos)
                  ref(defn.Predef_undefined)
                }
              val capture = if isByName then gen.evaluateByName(rawCapture) else rawCapture
              val capturedValue = gen.derefCapturedVar(capture, variable)
              gen.boxIfValueClass(variable, capturedValue)
            
            case ReflectEvalStrategy.ClassCaptureAssign(variable, cls) =>
              val capture = gen
                .getClassCapture(tree)(qualifier, variable.name, cls)
                .getOrElse {
                  report.error(s"No capture found for $variable in $cls", tree.srcPos)
                  ref(defn.Predef_undefined)
                }
              val value = gen.unboxIfValueClass(variable, args.head)
              val typeSymbol = variable.info.typeSymbol
              val elemField = typeSymbol.info.decl(elemName).symbol
              gen.setField(tree)(capture, elemField.asTerm, value)
            
            case ReflectEvalStrategy.MethodCapture(variable, method, isByName) =>
              val rawCapture = gen
                .getMethodCapture(method, variable.name)
                .getOrElse {
                  report.error(s"No capture found for $variable in $method", tree.srcPos)
                  ref(defn.Predef_undefined)
                }
              val capture = if isByName then gen.evaluateByName(rawCapture) else rawCapture
              val capturedValue = gen.derefCapturedVar(capture, variable)
              gen.boxIfValueClass(variable, capturedValue)
            
            case ReflectEvalStrategy.MethodCaptureAssign(variable, method) =>
              val capture = gen
                .getMethodCapture(method, variable.name)
                .getOrElse {
                  report.error(s"No capture found for $variable in $method", tree.srcPos)
                  ref(defn.Predef_undefined)
                }
              val value = gen.unboxIfValueClass(variable, args.head)
              val typeSymbol = variable.info.typeSymbol
              val elemField = typeSymbol.info.decl(elemName).symbol
              gen.setField(tree)(capture, elemField.asTerm, value)
            
            case ReflectEvalStrategy.StaticObject(obj) => gen.getStaticObject(obj)
            
            case ReflectEvalStrategy.Field(field, isByName) =>
              // if the field is lazy, if it is private in a value class or a trait
              // then we must call the getter method
              val fieldValue =
                if field.is(Lazy) || field.owner.isValueClass || field.owner.is(Trait)
                then gen.callMethod(tree)(qualifier, field.getter.asTerm, Nil)
                else
                  val rawValue = gen.getField(tree)(qualifier, field)
                  if isByName then gen.evaluateByName(rawValue) else rawValue
              gen.boxIfValueClass(field, fieldValue)
            
            case ReflectEvalStrategy.FieldAssign(field) =>
              val arg = gen.unboxIfValueClass(field, args.head)
              if field.owner.is(Trait) then
                gen.callMethod(tree)(qualifier, field.setter.asTerm, List(arg))
              else gen.setField(tree)(qualifier, field, arg)
            
            case ReflectEvalStrategy.MethodCall(method) => gen.callMethod(tree)(qualifier, method, args)
            case ReflectEvalStrategy.ConstructorCall(ctor, cls) => gen.callConstructor(tree)(qualifier, ctor, args)
        case _ => super.transform(tree)

  private def isReflectEval(symbol: Symbol)(using Context): Boolean =
    symbol.name == reflectEvalName && symbol.owner == config.expressionClass

  class Gen(reflectEval: Apply)(using Context):
    private val expressionThis = reflectEval.fun.asInstanceOf[Select].qualifier

    def derefCapturedVar(tree: Tree, term: TermSymbol): Tree =
      val typeSymbol = term.info.typeSymbol.asType
      JavaEncoding.encode(typeSymbol) match
        case s"scala.runtime.${_}Ref" =>
          val elemField = typeSymbol.info.decl(elemName).symbol
          getField(tree)(tree, elemField.asTerm)
        case _ => tree

    def boxIfValueClass(term: TermSymbol, tree: Tree): Tree =
      getErasedValueType(atPhase(Phases.elimErasedValueTypePhase)(term.info)) match
        case Some(erasedValueType) =>
          boxValueClass(erasedValueType.tycon.typeSymbol.asClass, tree)
        case None => tree

    def boxValueClass(valueClass: ClassSymbol, tree: Tree): Tree =
      // qualifier is null: a value class cannot be nested into a class
      val ctor = valueClass.primaryConstructor.asTerm
      callConstructor(tree)(nullLiteral, ctor, List(tree))

    def unboxIfValueClass(term: TermSymbol, tree: Tree): Tree =
      getErasedValueType(atPhase(Phases.elimErasedValueTypePhase)(term.info)) match
        case Some(erasedValueType) => unboxValueClass(tree, erasedValueType)
        case None => tree

    private def getErasedValueType(tpe: Type): Option[ErasedValueType] = tpe match
      case tpe: ErasedValueType => Some(tpe)
      case tpe: MethodOrPoly => getErasedValueType(tpe.resultType)
      case tpe => None

    private def unboxValueClass(tree: Tree, tpe: ErasedValueType): Tree =
      val cls = tpe.tycon.typeSymbol.asClass
      val unboxMethod = ValueClasses.valueClassUnbox(cls).asTerm
      callMethod(tree)(tree, unboxMethod, Nil)

    def getThisObject: Tree =
      Apply(Select(expressionThis, termName("getThisObject")), Nil)

    def getLocalValue(name: String): Tree =
      Apply(
        Select(expressionThis, termName("getLocalValue")),
        List(Literal(Constant(name)))
      )

    def setLocalValue(name: String, value: Tree): Tree =
      Apply(
        Select(expressionThis, termName("setLocalValue")),
        List(Literal(Constant(name)), value)
      )

    def getOuter(qualifier: Tree, outerCls: ClassSymbol): Tree =
      Apply(
        Select(expressionThis, termName("getOuter")),
        List(qualifier, Literal(Constant(JavaEncoding.encode(outerCls))))
      )

    def getClassCapture(tree: Tree)(qualifier: Tree, originalName: Name, cls: ClassSymbol): Option[Tree] =
      cls.info.decls.iterator
        .filter(term => term.isField)
        .find { field =>
          field.name match
            case DerivedName(underlying, _) if field.isPrivate =>
              underlying == originalName
            case DerivedName(DerivedName(_, info: QualifiedInfo), _) =>
              info.name == originalName
            case _ => false
        }
        .map(field => getField(tree: Tree)(qualifier, field.asTerm))

    def getMethodCapture(method: TermSymbol, originalName: TermName): Option[Tree] =
      val methodType = method.info.asInstanceOf[MethodType]
      methodType.paramNames
        .collectFirst { case name @ DerivedName(n, _) if n == originalName => name }
        .map(param => getLocalValue(JavaEncoding.encode(param)))

    def getStaticObject(obj: ClassSymbol): Tree =
      Apply(
        Select(expressionThis, termName("getStaticObject")),
        List(Literal(Constant(JavaEncoding.encode(obj))))
      )

    def getField(tree: Tree)(qualifier: Tree, field: TermSymbol): Tree =
      if field.owner.isTerm then
        report.error(s"Cannot access local val ${field.name} in ${field.owner} as field", tree.srcPos)
        ref(defn.Predef_undefined)
      else
        Apply(
          Select(expressionThis, termName("getField")),
          List(
            qualifier,
            Literal(Constant(JavaEncoding.encode(field.owner.asType))),
            Literal(Constant(JavaEncoding.encode(field.name)))
          )
        )

    def setField(tree: Tree)(qualifier: Tree, field: TermSymbol, value: Tree): Tree =
      if field.owner.isTerm then
        report.error(s"Cannot access local var ${field.name} in ${field.owner} as field", tree.srcPos)
        ref(defn.Predef_undefined)
      else
        Apply(
          Select(expressionThis, termName("setField")),
          List(
            qualifier,
            Literal(Constant(JavaEncoding.encode(field.owner.asType))),
            Literal(Constant(JavaEncoding.encode(field.name))),
            value
          )
        )

    def evaluateByName(function: Tree): Tree =
      val castFunction = function.cast(defn.Function0.typeRef.appliedTo(defn.AnyType))
      Apply(Select(castFunction, termName("apply")), List())

    def callMethod(tree: Tree)(qualifier: Tree, method: TermSymbol, args: List[Tree]): Tree =
      val methodType = method.info.asInstanceOf[MethodType]
      val paramTypesNames = methodType.paramInfos.map(JavaEncoding.encode)
      val paramTypesArray = JavaSeqLiteral(
        paramTypesNames.map(t => Literal(Constant(t))),
        TypeTree(ctx.definitions.StringType)
      )

      def unknownCapture(name: Name): Tree =
        report.error(s"Unknown captured variable $name in $method", reflectEval.srcPos)
        ref(defn.Predef_undefined)
      val capturedArgs = methodType.paramNames.dropRight(args.size).map {
        case name @ DerivedName(underlying, _) => capturedValue(tree)(method, underlying).getOrElse(unknownCapture(name))
        case name => unknownCapture(name)
      }

      val erasedMethodInfo = atPhase(Phases.elimErasedValueTypePhase)(method.info).asInstanceOf[MethodType]
      val unboxedArgs = erasedMethodInfo.paramInfos.takeRight(args.size).zip(args).map {
        case (tpe: ErasedValueType, arg) => unboxValueClass(arg, tpe)
        case (_, arg) => arg
      }

      val returnTypeName = JavaEncoding.encode(methodType.resType)
      val methodName = JavaEncoding.encode(method.name)
      val result = Apply(
        Select(expressionThis, termName("callMethod")),
        List(
          qualifier,
          Literal(Constant(JavaEncoding.encode(method.owner.asType))),
          Literal(Constant(methodName)),
          paramTypesArray,
          Literal(Constant(returnTypeName)),
          JavaSeqLiteral(capturedArgs ++ unboxedArgs, TypeTree(ctx.definitions.ObjectType))
        )
      )
      erasedMethodInfo.resType match
        case tpe: ErasedValueType => boxValueClass(tpe.tycon.typeSymbol.asClass, result)
        case _ => result
    end callMethod

    def callConstructor(tree: Tree)(qualifier: Tree, ctor: TermSymbol, args: List[Tree]): Tree =
      val methodType = ctor.info.asInstanceOf[MethodType]
      val paramTypesNames = methodType.paramInfos.map(JavaEncoding.encode)
      val clsName = JavaEncoding.encode(methodType.resType)

      val capturedArgs =
        methodType.paramNames.dropRight(args.size).map {
          case outer if outer == nme.OUTER => qualifier
          case name @ DerivedName(underlying, _) =>
            // if derived then probably a capture
            capturedValue(tree: Tree)(ctor.owner, underlying)
              .getOrElse {
                report.error(s"Unknown captured variable $name in $ctor of ${ctor.owner}", reflectEval.srcPos)
                ref(defn.Predef_undefined)
              }
          case name =>
            val paramName = JavaEncoding.encode(name)
            getLocalValue(paramName)
        }

      val erasedCtrInfo = atPhase(Phases.elimErasedValueTypePhase)(ctor.info)
        .asInstanceOf[MethodType]
      val unboxedArgs = erasedCtrInfo.paramInfos.takeRight(args.size).zip(args).map {
        case (tpe: ErasedValueType, arg) => unboxValueClass(arg, tpe)
        case (_, arg) => arg
      }

      val paramTypesArray = JavaSeqLiteral(
        paramTypesNames.map(t => Literal(Constant(t))),
        TypeTree(ctx.definitions.StringType)
      )
      Apply(
        Select(expressionThis, termName("callConstructor")),
        List(
          Literal(Constant(clsName)),
          paramTypesArray,
          JavaSeqLiteral(capturedArgs ++ unboxedArgs, TypeTree(ctx.definitions.ObjectType))
        )
      )
    end callConstructor

    private def capturedValue(tree: Tree)(sym: Symbol, originalName: TermName): Option[Tree] =
      val encodedName = JavaEncoding.encode(originalName)
      if expressionStore.classOwners.contains(sym) then capturedByClass(tree: Tree)(sym.asClass, originalName)
      else if config.localVariables.contains(encodedName) then Some(getLocalValue(encodedName))
      else
        // if the captured value is not a local variables
        // then it must have been captured by the outer method
        expressionStore.capturingMethod.flatMap(getMethodCapture(_, originalName))

    private def capturedByClass(tree: Tree)(cls: ClassSymbol, originalName: TermName): Option[Tree] =
      val target = expressionStore.classOwners.indexOf(cls)
      val qualifier = expressionStore.classOwners
        .drop(1)
        .take(target)
        .foldLeft(getThisObject)((q, cls) => getOuter(q, cls))
      getClassCapture(tree: Tree)(qualifier, originalName, cls)

private object ResolveReflectEval:
  val name = "resolvReflectEval"
