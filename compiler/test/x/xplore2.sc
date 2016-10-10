package x
import dotty.tools.dotc._
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
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val c = Main.newCompiler                        //> c  : dotty.tools.dotc.Compiler = dotty.tools.dotc.Compiler@d8d9850
  val base = new ContextBase                      //> base  : dotty.tools.dotc.core.Contexts.ContextBase = dotty.tools.dotc.core.C
                                                  //| ontexts$ContextBase@31ad98ef
  implicit val ctx = c.rootContext(base.initialCtx)
                                                  //> ctx  : dotty.tools.dotc.core.Contexts.Context = Context(
                                                  //|   owner = module class <root>, scope = Scopes()
                                                  //|   owner = module class <root>, scope = Scopes()
                                                  //|   owner = module class <root>, scope = Scopes()
                                                  //|   owner = module class <root>, scope = Scopes()
                                                  //|   owner = module class <root>, scope = Scopes()
                                                  //|   owner = val <none>, scope = null
  
  val SerializableClass = ctx.requiredClass("scala.Serializable")
                                                  //> SerializableClass  : dotty.tools.dotc.core.Symbols.ClassSymbol = class Seria
                                                  //| lizable
  val SystemModule = ctx.requiredModule("java.lang.System")
                                                  //> SystemModule  : dotty.tools.dotc.core.Symbols.TermSymbol = module System

  val scope = new Scopes.MutableScope             //> scope  : dotty.tools.dotc.core.Scopes.MutableScope = Scopes()
  val bla = ctx.newClassSymbol(   // suggestion: Use newCompleteClassSymbol instead!
    owner = defn.EmptyPackageClass,
    name = "Bia".toTypeName,
    flags = EmptyFlags,
    infoFn = cls => ClassInfo(
    	prefix = defn.EmptyPackageClass.thisType,
    	cls = cls,
    	classParents = List(defn.ObjectClass.typeRef, SerializableClass.typeRef),
    	decls = scope,
		  selfInfo = NoType)).entered     //> bla  : dotty.tools.dotc.core.Symbols.ClassSymbol = class Bia
  val constr = ctx.newConstructor(bla, EmptyFlags, List("p".toTermName, "r".toTermName), List(defn.AnyRefType, defn.IntType)).entered
                                                  //> constr  : dotty.tools.dotc.core.Symbols.Symbol{type ThisName = dotty.tools.
                                                  //| dotc.core.Names.TermName} = method <init>
  val foo = ctx.newSymbol(
    owner = bla,
    name = "foo".toTermName,
    flags = Final,
    info = MethodType(List("s".toTermName), List(defn.IntType), defn.UnitType)).entered
                                                  //> foo  : dotty.tools.dotc.core.Symbols.Symbol{type ThisName = dotty.tools.dot
                                                  //| c.core.Names.TermName} = val foo
  val rep = ctx.newSymbol(bla, "rep".toTermName, flags = Lazy, info = defn.IntType).entered
                                                  //> rep  : dotty.tools.dotc.core.Symbols.Symbol{type ThisName = dotty.tools.dot
                                                  //| c.core.Names.TermName} = val rep
  val tostr = ctx.newSymbol(bla, "toString".toTermName, flags = Override, info = defn.UnitType).entered
                                                  //> tostr  : dotty.tools.dotc.core.Symbols.Symbol{type ThisName = dotty.tools.d
                                                  //| otc.core.Names.TermName} = val toString
//  val repTree = ValDef(rep, Literal(Constant(3)))
  "==".toTermName.encode                          //> res0: dotty.tools.dotc.core.Names.Name = $eq$eq
  val b = ctx.newSymbol(tostr, "b".toTermName, EmptyFlags, bla.typeRef)
                                                  //> b  : dotty.tools.dotc.core.Symbols.Symbol{type ThisName = dotty.tools.dotc.
                                                  //| core.Names.TermName} = val b
 
  val bTree = ValDef(b, New(bla.typeRef, List(This(bla), Literal(Constant(3)))))
                                                  //> bTree  : dotty.tools.dotc.ast.tpd.ValDef = ValDef(Modifiers(,,List()),b,Typ
                                                  //| eTree[TypeRef(ThisType(module class <empty>),Bia)],Apply(Select(New(TypeTre
                                                  //| e[TypeRef(ThisType(module class <empty>),Bia)]),<init>),List(This(Bia), Lit
                                                  //| eral(Constant(3)))))
  val system = tpd.Ident(SystemModule.termRef)    //> system  : dotty.tools.dotc.ast.tpd.Ident = Ident(System)

  val ref @ TermRef(_, _) = system.tpe            //> ref  : dotty.tools.dotc.core.Types.TermRef = TermRef(ThisType(module class 
                                                  //| lang),System)
  ref.symbol                                      //> res1: dotty.tools.dotc.core.Symbols.Symbol = module System
  val idHash = Select(system, "identityHashCode".toTermName)
                                                  //> idHash  : dotty.tools.dotc.ast.tpd.Select = Select(Ident(System),identityHa
                                                  //| shCode)
  val idHashApp = Apply(idHash, Ident(b.termRef) :: Nil)
                                                  //> idHashApp  : dotty.tools.dotc.ast.tpd.Apply = Apply(Select(Ident(System),id
                                                  //| entityHashCode),List(Ident(b)))
  val eqlref = defn.IntType.member("==".toTermName)
                                                  //> eqlref  : dotty.tools.dotc.core.Denotations.Denotation = val <none>
  
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