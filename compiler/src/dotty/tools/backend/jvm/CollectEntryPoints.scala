package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.transform.MegaPhase._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc
import dotty.tools.dotc.backend.jvm.DottyPrimitives
import dotty.tools.dotc.core.Flags.FlagSet
import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.transform.SymUtils._
import java.io.{File => JFile}

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import dotty.tools.io.{Directory, PlainDirectory, AbstractFile}
import scala.tools.asm.{ClassVisitor, FieldVisitor, MethodVisitor}
import scala.tools.nsc.backend.jvm.{BCodeHelpers, BackendInterface}
import dotty.tools.dotc.core._
import Periods._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Denotations._
import Phases._
import java.lang.AssertionError
import dotty.tools.dotc.util.Positions.Position
import Decorators._
import tpd._
import StdNames.nme

/**
 * Created by dark on 26/11/14.
 */
class CollectEntryPoints extends MiniPhase {
  def phaseName: String = "Collect entry points"

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree = {
    if ((tree.symbol ne NoSymbol) && CollectEntryPoints.isJavaEntryPoint(tree.symbol)) {
      ctx.genBCodePhase.asInstanceOf[GenBCode].registerEntryPoint(tree.symbol)
    }
    tree
  }
}

object CollectEntryPoints{
  def isJavaMainMethod(sym: Symbol)(implicit ctx: Context) = {
    (sym.name == nme.main) && (sym.info match {
      case r@MethodTpe(_, List(defn.ArrayOf(t)), _) =>
        (t.widenDealias =:= defn.StringType) && (
        r.resultType.widenDealias =:= defn.UnitType)
      case _ => false
    })
  }

  def isJavaEntryPoint(sym: Symbol)(implicit ctx: Context): Boolean = {
    import Types.MethodType
    val d = ctx.definitions
    val StringType = d.StringType
    // The given class has a main method.
    def hasJavaMainMethod(sym: Symbol): Boolean =
      (toDenot(sym).info member nme.main).alternatives exists(x => isJavaMainMethod(x.symbol))

    def fail(msg: String, pos: Position = sym.pos) = {
      ctx.warning(          sym.name +
        s" has a main method with parameter type Array[String], but ${toDenot(sym).fullName} will not be a runnable program.\n  Reason: $msg",
        sym.pos
        // TODO: make this next claim true, if possible
        //   by generating valid main methods as static in module classes
        //   not sure what the jvm allows here
        // + "  You can still run the program by calling it as " + javaName(sym) + " instead."
      )
      false
    }
    def failNoForwarder(msg: String) = {
      fail(s"$msg, which means no static forwarder can be generated.\n")
    }
    val possibles = if (sym.flags is Flags.Module) (toDenot(sym).info nonPrivateMember nme.main).alternatives else Nil
    val hasApproximate = possibles exists { m =>
      m.info match {
        case MethodTpe(_, p :: Nil, _) => p.typeSymbol == defn.ArrayClass
        case _                         => false
      }
    }
    // At this point it's a module with a main-looking method, so either succeed or warn that it isn't.
    hasApproximate && {
      // Before erasure so we can identify generic mains.
      {
        // implicit val c = ctx.withPhase(ctx.erasurePhase)

        val companion     = sym.asClass.moduleClass

        if (hasJavaMainMethod(companion))
          failNoForwarder("companion contains its own main method")
        else if (toDenot(companion).info.member(nme.main) != NoDenotation)
        // this is only because forwarders aren't smart enough yet
          failNoForwarder("companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
        else if (companion.flags is Flags.Trait)
          failNoForwarder("companion is a trait")
        // Now either succeed, or issue some additional warnings for things which look like
        // attempts to be java main methods.
        else (possibles exists(x=> isJavaMainMethod(x.symbol))) || {
          possibles exists { m =>
            toDenot(m.symbol).info match {
              case t: PolyType =>
                fail("main methods cannot be generic.")
              case MethodTpe(paramNames, paramTypes, resultType) =>
                if (resultType :: paramTypes exists (_.typeSymbol.isAbstractType))
                  fail("main methods cannot refer to type parameters or abstract types.", m.symbol.pos)
                else
                  isJavaMainMethod(m.symbol) || fail("main method must have exact signature (Array[String])Unit", m.symbol.pos)
              case tp =>
                fail(s"don't know what this is: $tp", m.symbol.pos)
            }
          }
        }
      }
    }
  }
}
