package x
import dotty.compiler.internal._
import core._
import Contexts._
import Symbols._
import Decorators._  // should always import to get useful implicit decorators.
import Flags._
import Types._
import ast.Trees._
import ast.tpd
import ast.untpd
import Constants._

object xplore2 {
  import ast.tpd._
  println("Welcome to the Scala worksheet")
  val c = Main.newCompiler
  val base = new ContextBase
  implicit val ctx = c.rootContext(base.initialCtx)
  
  val SerializableClass = ctx.requiredClass("scala.Serializable")
  val SystemModule = ctx.requiredModule("java.lang.System")

  val scope = new Scopes.MutableScope
  val bla = ctx.newClassSymbol(   // suggestion: Use newCompleteClassSymbol instead!
    owner = defn.EmptyPackageClass,
    name = "Bia".toTypeName,
    flags = EmptyFlags,
    infoFn = cls => ClassInfo(
    	prefix = defn.EmptyPackageClass.thisType,
    	cls = cls,
    	classParents = List(defn.ObjectClass.typeRef, SerializableClass.typeRef),
    	decls = scope,
		  selfInfo = NoType)).entered
  val constr = ctx.newConstructor(bla, EmptyFlags, List("p".toTermName, "r".toTermName), List(defn.AnyRefType, defn.IntType)).entered
  val foo = ctx.newSymbol(
    owner = bla,
    name = "foo".toTermName,
    flags = Final,
    info = MethodType(List("s".toTermName), List(defn.IntType), defn.UnitType)).entered
  val rep = ctx.newSymbol(bla, "rep".toTermName, flags = Lazy, info = defn.IntType).entered
  val tostr = ctx.newSymbol(bla, "toString".toTermName, flags = Override, info = defn.UnitType).entered
//  val repTree = ValDef(rep, Literal(Constant(3)))
  "==".toTermName.encode
  val b = ctx.newSymbol(tostr, "b".toTermName, EmptyFlags, bla.typeRef)
 
  val bTree = ValDef(b, New(bla.typeRef, List(This(bla), Literal(Constant(3)))))
  val system = tpd.Ident(SystemModule.termRef)

  val ref @ TermRef(_, _) = system.tpe
  ref.symbol
  val idHash = Select(system, "identityHashCode".toTermName)
  val idHashApp = Apply(idHash, Ident(b.termRef) :: Nil)
  val eqlref = defn.IntType.member("==".toTermName)
  
    /*
  val tostrTree = {
    Block(
      List(
      	ValDef(b, New(bla.typeRef, List(This(bla), Literal(Constant(3))))),
      	If(
        	Apply(
          	Select(
		        	Apply(
    		      	Select(Ident(SystemModule.termRef), "identityHashCode".toTermName),
        		  	Ident(b.termRef) :: Nil),
        			"==".toTermName.encode),
        			Literal(Constant(0)) :: Nil),
        	Apply(Select(Ident(b.termRef), foo.termRef), Literal(Constant(3)) :: Nil),
        	EmptyTree)),
     Literal(Constant(())))
   }*/
}