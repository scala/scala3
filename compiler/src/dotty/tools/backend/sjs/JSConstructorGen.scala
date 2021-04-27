package dotty.tools.backend.sjs

import org.scalajs.ir
import org.scalajs.ir.{Position, Trees => js, Types => jstpe}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName

import JSCodeGen.UndefinedParam

object JSConstructorGen {

  /** Builds one JS constructor out of several "init" methods and their
   *  dispatcher.
   *
   *  This method and the rest of this file are copied verbatim from `GenJSCode`
   *  for scalac, since there is no dependency on the compiler trees/symbols/etc.
   *  We are only manipulating IR trees and types.
   *
   *  The only difference is the two parameters `overloadIdent` and `reportError`,
   *  which are added so that this entire file can be even more isolated.
   */
  def buildJSConstructorDef(dispatch: js.JSMethodDef, ctors: List[js.MethodDef],
      overloadIdent: js.LocalIdent)(
      reportError: String => Unit)(
      implicit pos: Position): js.JSMethodDef = {

    val js.JSMethodDef(_, dispatchName, dispatchArgs, dispatchRestParam, dispatchResolution) =
      dispatch

    val jsConstructorBuilder = mkJSConstructorBuilder(ctors, reportError)

    // Section containing the overload resolution and casts of parameters
    val overloadSelection = mkOverloadSelection(jsConstructorBuilder,
      overloadIdent, dispatchResolution)

    /* Section containing all the code executed before the call to `this`
     * for every secondary constructor.
     */
    val prePrimaryCtorBody =
      jsConstructorBuilder.mkPrePrimaryCtorBody(overloadIdent)

    val primaryCtorBody = jsConstructorBuilder.primaryCtorBody

    /* Section containing all the code executed after the call to this for
     * every secondary constructor.
     */
    val postPrimaryCtorBody =
      jsConstructorBuilder.mkPostPrimaryCtorBody(overloadIdent)

    val newBody = js.Block(overloadSelection ::: prePrimaryCtorBody ::
        primaryCtorBody :: postPrimaryCtorBody :: js.Undefined() :: Nil)

    js.JSMethodDef(js.MemberFlags.empty, dispatchName, dispatchArgs, dispatchRestParam, newBody)(
        dispatch.optimizerHints, None)
  }

  private class ConstructorTree(val overrideNum: Int, val method: js.MethodDef,
      val subConstructors: List[ConstructorTree]) {

    lazy val overrideNumBounds: (Int, Int) =
      if (subConstructors.isEmpty) (overrideNum, overrideNum)
      else (subConstructors.head.overrideNumBounds._1, overrideNum)

    def get(methodName: MethodName): Option[ConstructorTree] = {
      if (methodName == this.method.methodName) {
        Some(this)
      } else {
        subConstructors.iterator.map(_.get(methodName)).collectFirst {
          case Some(node) => node
        }
      }
    }

    def getParamRefs(implicit pos: Position): List[js.VarRef] =
      method.args.map(_.ref)

    def getAllParamDefsAsVars(implicit pos: Position): List[js.VarDef] = {
      val localDefs = method.args.map { pDef =>
        js.VarDef(pDef.name, pDef.originalName, pDef.ptpe, mutable = true,
            jstpe.zeroOf(pDef.ptpe))
      }
      localDefs ++ subConstructors.flatMap(_.getAllParamDefsAsVars)
    }
  }

  private class JSConstructorBuilder(root: ConstructorTree, reportError: String => Unit) {

    def primaryCtorBody: js.Tree = root.method.body.getOrElse(
        throw new AssertionError("Found abstract constructor"))

    def hasSubConstructors: Boolean = root.subConstructors.nonEmpty

    def getOverrideNum(methodName: MethodName): Int =
      root.get(methodName).fold(-1)(_.overrideNum)

    def getParamRefsFor(methodName: MethodName)(implicit pos: Position): List[js.VarRef] =
      root.get(methodName).fold(List.empty[js.VarRef])(_.getParamRefs)

    def getAllParamDefsAsVars(implicit pos: Position): List[js.VarDef] =
      root.getAllParamDefsAsVars

    def mkPrePrimaryCtorBody(overrideNumIdent: js.LocalIdent)(
        implicit pos: Position): js.Tree = {
      val overrideNumRef = js.VarRef(overrideNumIdent)(jstpe.IntType)
      mkSubPreCalls(root, overrideNumRef)
    }

    def mkPostPrimaryCtorBody(overrideNumIdent: js.LocalIdent)(
        implicit pos: Position): js.Tree = {
      val overrideNumRef = js.VarRef(overrideNumIdent)(jstpe.IntType)
      js.Block(mkSubPostCalls(root, overrideNumRef))
    }

    private def mkSubPreCalls(constructorTree: ConstructorTree,
        overrideNumRef: js.VarRef)(implicit pos: Position): js.Tree = {
      val overrideNumss = constructorTree.subConstructors.map(_.overrideNumBounds)
      val paramRefs = constructorTree.getParamRefs
      val bodies = constructorTree.subConstructors.map { constructorTree =>
        mkPrePrimaryCtorBodyOnSndCtr(constructorTree, overrideNumRef, paramRefs)
      }
      overrideNumss.zip(bodies).foldRight[js.Tree](js.Skip()) {
        case ((numBounds, body), acc) =>
          val cond = mkOverrideNumsCond(overrideNumRef, numBounds)
          js.If(cond, body, acc)(jstpe.BooleanType)
      }
    }

    private def mkPrePrimaryCtorBodyOnSndCtr(constructorTree: ConstructorTree,
        overrideNumRef: js.VarRef, outputParams: List[js.VarRef])(
        implicit pos: Position): js.Tree = {
      val subCalls =
        mkSubPreCalls(constructorTree, overrideNumRef)

      val preSuperCall = {
        def checkForUndefinedParams(args: List[js.Tree]): List[js.Tree] = {
          def isUndefinedParam(tree: js.Tree): Boolean = tree match {
            case js.Transient(UndefinedParam) => true
            case _                            => false
          }

          if (!args.exists(isUndefinedParam)) {
            args
          } else {
            /* If we find an undefined param here, we're in trouble, because
             * the handling of a default param for the target constructor has
             * already been done during overload resolution. If we store an
             * `undefined` now, it will fall through without being properly
             * processed.
             *
             * Since this seems very tricky to deal with, and a pretty rare
             * use case (with a workaround), we emit an "implementation
             * restriction" error.
             */
            reportError(
                "Implementation restriction: in a JS class, a secondary " +
                "constructor calling another constructor with default " +
                "parameters must provide the values of all parameters.")

            /* Replace undefined params by undefined to prevent subsequent
             * compiler crashes.
             */
            args.map { arg =>
              if (isUndefinedParam(arg))
                js.Undefined()(arg.pos)
              else
                arg
            }
          }
        }

        constructorTree.method.body.get match {
          case js.Block(stats) =>
            val beforeSuperCall = stats.takeWhile {
              case js.ApplyStatic(_, _, mtd, _) => !mtd.name.isConstructor
              case _                            => true
            }
            val superCallParams = stats.collectFirst {
              case js.ApplyStatic(_, _, mtd, js.This() :: args)
                  if mtd.name.isConstructor =>
                val checkedArgs = checkForUndefinedParams(args)
                zipMap(outputParams, checkedArgs)(js.Assign(_, _))
            }.getOrElse(Nil)

            beforeSuperCall ::: superCallParams

          case js.ApplyStatic(_, _, mtd, js.This() :: args)
              if mtd.name.isConstructor =>
            val checkedArgs = checkForUndefinedParams(args)
            zipMap(outputParams, checkedArgs)(js.Assign(_, _))

          case _ => Nil
        }
      }

      js.Block(subCalls :: preSuperCall)
    }

    private def mkSubPostCalls(constructorTree: ConstructorTree,
        overrideNumRef: js.VarRef)(implicit pos: Position): js.Tree = {
      val overrideNumss = constructorTree.subConstructors.map(_.overrideNumBounds)
      val bodies = constructorTree.subConstructors.map { ct =>
        mkPostPrimaryCtorBodyOnSndCtr(ct, overrideNumRef)
      }
      overrideNumss.zip(bodies).foldRight[js.Tree](js.Skip()) {
        case ((numBounds, js.Skip()), acc) => acc

        case ((numBounds, body), acc) =>
          val cond = mkOverrideNumsCond(overrideNumRef, numBounds)
          js.If(cond, body, acc)(jstpe.BooleanType)
      }
    }

    private def mkPostPrimaryCtorBodyOnSndCtr(constructorTree: ConstructorTree,
        overrideNumRef: js.VarRef)(implicit pos: Position): js.Tree = {
      val postSuperCall = {
        constructorTree.method.body.get match {
          case js.Block(stats) =>
            stats.dropWhile {
              case js.ApplyStatic(_, _, mtd, _) => !mtd.name.isConstructor
              case _                            => true
            }.tail

          case _ => Nil
        }
      }
      js.Block(postSuperCall :+ mkSubPostCalls(constructorTree, overrideNumRef))
    }

    private def mkOverrideNumsCond(numRef: js.VarRef,
        numBounds: (Int, Int))(implicit pos: Position) = numBounds match {
      case (lo, hi) if lo == hi =>
        js.BinaryOp(js.BinaryOp.Int_==, js.IntLiteral(lo), numRef)

      case (lo, hi) if lo == hi - 1 =>
        val lhs = js.BinaryOp(js.BinaryOp.Int_==, numRef, js.IntLiteral(lo))
        val rhs = js.BinaryOp(js.BinaryOp.Int_==, numRef, js.IntLiteral(hi))
        js.If(lhs, js.BooleanLiteral(true), rhs)(jstpe.BooleanType)

      case (lo, hi) =>
        val lhs = js.BinaryOp(js.BinaryOp.Int_<=, js.IntLiteral(lo), numRef)
        val rhs = js.BinaryOp(js.BinaryOp.Int_<=, numRef, js.IntLiteral(hi))
        js.BinaryOp(js.BinaryOp.Boolean_&, lhs, rhs)
        js.If(lhs, rhs, js.BooleanLiteral(false))(jstpe.BooleanType)
    }
  }

  private def zipMap[T, U, V](xs: List[T], ys: List[U])(
      f: (T, U) => V): List[V] = {
    for ((x, y) <- xs zip ys) yield f(x, y)
  }

  /** mkOverloadSelection return a list of `stats` with that starts with:
   *  1) The definition for the local variable that will hold the overload
   *     resolution number.
   *  2) The definitions of all local variables that are used as parameters
   *     in all the constructors.
   *  3) The overload resolution match/if statements. For each overload the
   *     overload number is assigned and the parameters are cast and assigned
   *     to their corresponding variables.
   */
  private def mkOverloadSelection(jsConstructorBuilder: JSConstructorBuilder,
      overloadIdent: js.LocalIdent, dispatchResolution: js.Tree)(
      implicit pos: Position): List[js.Tree] = {

    def deconstructApplyCtor(body: js.Tree): (List[js.Tree], MethodName, List[js.Tree]) = {
      val (prepStats, applyCtor) = (body: @unchecked) match {
        case applyCtor: js.ApplyStatic =>
          (Nil, applyCtor)
        case js.Block(prepStats :+ (applyCtor: js.ApplyStatic)) =>
          (prepStats, applyCtor)
      }
      val js.ApplyStatic(_, _, js.MethodIdent(ctorName), js.This() :: ctorArgs) =
        applyCtor
      assert(ctorName.isConstructor,
          s"unexpected super constructor call to non-constructor $ctorName at ${applyCtor.pos}")
      (prepStats, ctorName, ctorArgs)
    }

    if (!jsConstructorBuilder.hasSubConstructors) {
      val (prepStats, ctorName, ctorArgs) =
        deconstructApplyCtor(dispatchResolution)

      val refs = jsConstructorBuilder.getParamRefsFor(ctorName)
      assert(refs.size == ctorArgs.size, s"at $pos")
      val assignCtorParams = zipMap(refs, ctorArgs) { (ref, ctorArg) =>
        js.VarDef(ref.ident, NoOriginalName, ref.tpe, mutable = false, ctorArg)
      }

      prepStats ::: assignCtorParams
    } else {
      val overloadRef = js.VarRef(overloadIdent)(jstpe.IntType)

      /* transformDispatch takes the body of the method generated by
       * `genJSConstructorDispatch` and transform it recursively.
       */
      def transformDispatch(tree: js.Tree): js.Tree = tree match {
        // Parameter count resolution
        case js.Match(selector, cases, default) =>
          val newCases = cases.map {
            case (literals, body) => (literals, transformDispatch(body))
          }
          val newDefault = transformDispatch(default)
          js.Match(selector, newCases, newDefault)(tree.tpe)

        // Parameter type resolution
        case js.If(cond, thenp, elsep) =>
          js.If(cond, transformDispatch(thenp),
              transformDispatch(elsep))(tree.tpe)

        // Throw(StringLiteral(No matching overload))
        case tree: js.Throw =>
          tree

        // Overload resolution done, apply the constructor
        case _ =>
          val (prepStats, ctorName, ctorArgs) = deconstructApplyCtor(tree)

          val num = jsConstructorBuilder.getOverrideNum(ctorName)
          val overloadAssign = js.Assign(overloadRef, js.IntLiteral(num))

          val refs = jsConstructorBuilder.getParamRefsFor(ctorName)
          assert(refs.size == ctorArgs.size, s"at $pos")
          val assignCtorParams = zipMap(refs, ctorArgs)(js.Assign(_, _))

          js.Block(overloadAssign :: prepStats ::: assignCtorParams)
      }

      val newDispatchResolution = transformDispatch(dispatchResolution)
      val allParamDefsAsVars = jsConstructorBuilder.getAllParamDefsAsVars
      val overrideNumDef = js.VarDef(overloadIdent, NoOriginalName,
          jstpe.IntType, mutable = true, js.IntLiteral(0))

      overrideNumDef :: allParamDefsAsVars ::: newDispatchResolution :: Nil
    }
  }

  private def mkJSConstructorBuilder(ctors: List[js.MethodDef], reportError: String => Unit)(
      implicit pos: Position): JSConstructorBuilder = {
    def findCtorForwarderCall(tree: js.Tree): MethodName = (tree: @unchecked) match {
      case js.ApplyStatic(_, _, method, js.This() :: _)
          if method.name.isConstructor =>
        method.name

      case js.Block(stats) =>
        stats.collectFirst {
          case js.ApplyStatic(_, _, method, js.This() :: _)
              if method.name.isConstructor =>
            method.name
        }.get
    }

    val (primaryCtor :: Nil, secondaryCtors) = ctors.partition {
      _.body.get match {
        case js.Block(stats) =>
          stats.exists(_.isInstanceOf[js.JSSuperConstructorCall])

        case _: js.JSSuperConstructorCall => true
        case _                            => false
      }
    }

    val ctorToChildren = secondaryCtors.map { ctor =>
      findCtorForwarderCall(ctor.body.get) -> ctor
    }.groupBy(_._1).map(kv => kv._1 -> kv._2.map(_._2)).withDefaultValue(Nil)

    var overrideNum = -1
    def mkConstructorTree(method: js.MethodDef): ConstructorTree = {
      val subCtrTrees = ctorToChildren(method.methodName).map(mkConstructorTree)
      overrideNum += 1
      new ConstructorTree(overrideNum, method, subCtrTrees)
    }

    new JSConstructorBuilder(mkConstructorTree(primaryCtor), reportError: String => Unit)
  }

}
