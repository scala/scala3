package dotty.tools
package dotc
package transform
package sjs

import MegaPhase._
import core.Constants
import core.Contexts._
import core.Decorators._
import core.StdNames.nme
import core.Symbols._

import dotty.tools.backend.sjs.JSDefinitions.jsdefn

/** Adds fake calls to the constructors of local JS classes in calls to
 *  `createLocalJSClass`.
 *
 *  Given a call of the form
 *  {{{
 *  scala.scalajs.runtime.createLocalJSClass(classOf[C], jsClassValue, ???)
 *  }}}
 *  this phase fills in the `???` with an array of calls to the constructors
 *  of `C`, like
 *  {{{
 *  [ new C(), new C(???, ???) : Object ]
 *  }}}
 *
 *  If the class `C` has an outer pointer, as determined by the `ExplicitOuter`
 *  phase, the calls to the constructor insert a reference to the outer
 *  instance:
 *  {{{
 *  [ new C(Enclosing.this), new C(Enclosing.this, ???, ???) : Object ]
 *  }}}
 *
 *  The `LambdaLift` phase will further expand those constructor calls with
 *  values for captures. The back-end will extract the values of the outer
 *  pointer and/or the captures to introduce them as JS class captures.
 *
 *  Since we need to insert fake `new C()` calls, this scheme does not work for
 *  abstract local classes. We therefore reject them as implementation
 *  restriction in `PrepJSInterop`.
 *
 *  This phase complements `ExplicitJSClasses`. The latter cannot create the
 *  fake new invocations because that would require inventing sound type
 *  arguments for the class' type parameters in order not to break Ycheck.
 */
class AddLocalJSFakeNews extends MiniPhase { thisPhase =>
  import ExplicitOuter.outer
  import ast.tpd._

  override def phaseName: String = AddLocalJSFakeNews.name

  override def description: String = AddLocalJSFakeNews.description

  override def isEnabled(using Context): Boolean =
    ctx.settings.scalajs.value

  override def runsAfter: Set[String] = Set(Erasure.name)

  override def transformApply(tree: Apply)(using Context): Tree = {
    if (tree.symbol == jsdefn.Runtime_createLocalJSClass) {
      val classValueArg :: superClassValueArg :: _ :: Nil = tree.args
      val cls = classValueArg match {
        case Literal(constant) if constant.tag == Constants.ClazzTag =>
          constant.typeValue.typeSymbol.asClass
        case _ =>
          // this shouldn't happen
          report.error(i"unexpected $classValueArg for the first argument to `createLocalJSClass`", classValueArg)
          jsdefn.JSObjectClass
      }

      val fakeNews = {
        val ctors = cls.info.decls.lookupAll(nme.CONSTRUCTOR).toList.reverse
        val elems = ctors.map(ctor => fakeNew(cls, ctor.asTerm))
        JavaSeqLiteral(elems, TypeTree(defn.ObjectType))
      }

      cpy.Apply(tree)(tree.fun, classValueArg :: superClassValueArg :: fakeNews :: Nil)
    } else {
      tree
    }
  }

  /** Creates a fake invocation of the given class with the given constructor. */
  private def fakeNew(cls: ClassSymbol, ctor: TermSymbol)(using Context): Tree = {
    val tycon = cls.typeRef
    val outerArgs = outer.argsForNew(cls, tycon)
    val nonOuterArgCount = ctor.info.firstParamTypes.size - outerArgs.size
    val nonOuterArgs = List.fill(nonOuterArgCount)(ref(defn.Predef_undefined).appliedToNone)

    New(tycon, ctor, outerArgs ::: nonOuterArgs)
  }
}

object AddLocalJSFakeNews {
  val name: String = "addLocalJSFakeNews"
  val description: String = "adds fake new invocations to local JS classes in calls to `createLocalJSClass`"
}
