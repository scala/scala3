package dotty.tools
package dotc
package parsing

import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.FlagSet

import scala.language.implicitConversions

import JavaTokens._
import JavaScanners._
import Scanners.Offset
import Parsers._
import core._
import Contexts._
import Names._
import NameOps._
import Types._
import Symbols._
import ast.Trees._
import Decorators._
import StdNames._
import dotty.tools.dotc.reporting.diagnostic.messages.IdentifierExpected
import dotty.tools.dotc.util.SourceFile
import util.Positions._
import annotation.switch
import scala.collection.mutable.ListBuffer

object JavaParsers {

  import ast.untpd._

  class JavaParser(source: SourceFile)(implicit ctx: Context) extends ParserCommon(source) {

    val definitions = ctx.definitions
    import definitions._

    val in: JavaScanner = new JavaScanner(source)

    /** The simple name of the package of the currently parsed file */
    private[this] var thisPackageName: TypeName = tpnme.EMPTY

    /** This is the general parse entry point.
     *  Overridden by ScriptParser
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

    // -------- error handling ---------------------------------------

    protected def skip(): Unit = {
      var nparens = 0
      var nbraces = 0
      while (true) {
        in.token match {
          case EOF =>
            return
          case SEMI =>
            if (nparens == 0 && nbraces == 0) return
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            if (nbraces == 0) return
            nbraces -= 1
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        in.nextToken()
      }
    }

    def syntaxError(msg: String, skipIt: Boolean): Unit = {
      syntaxError(in.offset, msg, skipIt)
    }

    def syntaxError(pos: Int, msg: String, skipIt: Boolean): Unit = {
      if (pos > lastErrorOffset) {
        syntaxError(msg, pos)
        // no more errors on this token.
        lastErrorOffset = in.offset
      }
      if (skipIt)
        skip()
    }

    def errorTypeTree = TypeTree().withType(UnspecifiedErrorType) withPos Position(in.offset)

    // --------- tree building -----------------------------

    def scalaAnnotationDot(name: Name) = Select(scalaDot(nme.annotation), name)

    def javaDot(name: Name): Tree =
      Select(rootDot(nme.java), name)

    def javaLangDot(name: Name): Tree =
      Select(javaDot(nme.lang), name)

    def javaLangObject(): Tree = javaLangDot(tpnme.Object)

    def arrayOf(tpt: Tree) =
      AppliedTypeTree(Ident(nme.Array.toTypeName), List(tpt))

    def unimplementedExpr(implicit ctx: Context) =
      Select(Select(rootDot(nme.scala_), nme.Predef), nme.???)

    def makeTemplate(parents: List[Tree], stats: List[Tree], tparams: List[TypeDef], needsDummyConstr: Boolean) = {
      def pullOutFirstConstr(stats: List[Tree]): (Tree, List[Tree]) = stats match {
        case (meth: DefDef) :: rest if meth.name == nme.CONSTRUCTOR => (meth, rest)
        case first :: rest =>
          val (constr, tail) = pullOutFirstConstr(rest)
          (constr, first :: tail)
        case nil => (EmptyTree, nil)
      }
      var (constr1, stats1) = pullOutFirstConstr(stats)
      if (constr1 == EmptyTree) constr1 = makeConstructor(List(), tparams)
      // A dummy first constructor is needed for Java classes so that the real constructors see the
      // import of the companion object. The constructor has parameter of type Unit so no Java code
      // can call it.
      if (needsDummyConstr) {
        stats1 = constr1 :: stats1
        constr1 = makeConstructor(List(scalaDot(tpnme.Unit)), tparams, Flags.JavaDefined | Flags.Private | Flags.Local)
      }
      Template(constr1.asInstanceOf[DefDef], parents, EmptyValDef, stats1)
    }

    def makeSyntheticParam(count: Int, tpt: Tree): ValDef =
      makeParam(nme.syntheticParamName(count), tpt)
    def makeParam(name: TermName, tpt: Tree): ValDef =
      ValDef(name, tpt, EmptyTree).withMods(Modifiers(Flags.JavaDefined | Flags.ParamAccessor))

    def makeConstructor(formals: List[Tree], tparams: List[TypeDef], flags: FlagSet = Flags.JavaDefined) = {
      val vparams = formals.zipWithIndex.map { case (p, i) => makeSyntheticParam(i + 1, p) }
      DefDef(nme.CONSTRUCTOR, tparams, List(vparams), TypeTree(), EmptyTree).withMods(Modifiers(flags))
    }

    // ------------- general parsing ---------------------------

    /** skip parent or brace enclosed sequence of things */
    def skipAhead(): Unit = {
      var nparens = 0
      var nbraces = 0
      do {
        in.token match {
          case LPAREN =>
            nparens += 1
          case LBRACE =>
            nbraces += 1
          case _ =>
        }
        in.nextToken()
        in.token match {
          case RPAREN =>
            nparens -= 1
          case RBRACE =>
            nbraces -= 1
          case _ =>
        }
      } while (in.token != EOF && (nparens > 0 || nbraces > 0))
    }

    def skipTo(tokens: Int*): Unit = {
      while (!(tokens contains in.token) && in.token != EOF) {
        if (in.token == LBRACE) { skipAhead(); accept(RBRACE) }
        else if (in.token == LPAREN) { skipAhead(); accept(RPAREN) }
        else in.nextToken()
      }
    }

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      *
      * @return The offset at the start of the token to accept
      */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        val offsetToReport = in.offset
        val msg =
          tokenString(token) + " expected but " +
            tokenString(in.token) + " found."

        syntaxError(offsetToReport, msg, skipIt = true)
      }
      if (in.token == token) in.nextToken()
      offset
    }

    def acceptClosingAngle(): Unit = {
      val closers: PartialFunction[Int, Int] = {
        case GTGTGTEQ => GTGTEQ
        case GTGTGT   => GTGT
        case GTGTEQ   => GTEQ
        case GTGT     => GT
        case GTEQ     => EQUALS
      }
      if (closers isDefinedAt in.token) in.token = closers(in.token)
      else accept(GT)
    }

    def identForType(): TypeName = ident().toTypeName
    def ident(): Name =
      if (in.token == IDENTIFIER) {
        val name = in.name
        in.nextToken()
        name
      } else {
        accept(IDENTIFIER)
        nme.ERROR
      }

    def repsep[T <: Tree](p: () => T, sep: Int): List[T] = {
      val buf = ListBuffer[T](p())
      while (in.token == sep) {
        in.nextToken()
        buf += p()
      }
      buf.toList
    }

    /** Convert (qual)ident to type identifier
      */
    def convertToTypeId(tree: Tree): Tree = convertToTypeName(tree) match {
      case Some(t)  => t withPos tree.pos
      case _        => tree match {
        case AppliedTypeTree(_, _) | Select(_, _) =>
          tree
        case _ =>
          syntaxError(IdentifierExpected(tree.show), tree.pos)
          errorTypeTree
      }
    }

    /** Translate names in Select/Ident nodes to type names.
      */
    def convertToTypeName(tree: Tree): Option[RefTree] = tree match {
      case Select(qual, name) => Some(Select(qual, name.toTypeName))
      case Ident(name)        => Some(Ident(name.toTypeName))
      case _                  => None
    }
    // -------------------- specific parsing routines ------------------

    def qualId(): RefTree = {
      var t: RefTree = atPos(in.offset) { Ident(ident()) }
      while (in.token == DOT) {
        in.nextToken()
        t = atPos(t.pos.start, in.offset) { Select(t, ident()) }
      }
      t
    }

    def optArrayBrackets(tpt: Tree): Tree =
      if (in.token == LBRACKET) {
        val tpt1 = atPos(tpt.pos.start, in.offset) { arrayOf(tpt) }
        in.nextToken()
        accept(RBRACKET)
        optArrayBrackets(tpt1)
      } else tpt

    def basicType(): Tree =
      atPos(in.offset) {
        in.token match {
          case BYTE    => in.nextToken(); TypeTree(ByteType)
          case SHORT   => in.nextToken(); TypeTree(ShortType)
          case CHAR    => in.nextToken(); TypeTree(CharType)
          case INT     => in.nextToken(); TypeTree(IntType)
          case LONG    => in.nextToken(); TypeTree(LongType)
          case FLOAT   => in.nextToken(); TypeTree(FloatType)
          case DOUBLE  => in.nextToken(); TypeTree(DoubleType)
          case BOOLEAN => in.nextToken(); TypeTree(BooleanType)
          case _       => syntaxError("illegal start of type", skipIt = true); errorTypeTree
        }
      }

    def typ(): Tree =
      optArrayBrackets {
        if (in.token == FINAL) in.nextToken()
        if (in.token == IDENTIFIER) {
          var t = typeArgs(atPos(in.offset)(Ident(ident())))
          // typeSelect generates Select nodes if the lhs is an Ident or Select,
          // For other nodes it always assumes that the selected item is a type.
          def typeSelect(t: Tree, name: Name) = t match {
            case Ident(_) | Select(_, _) => Select(t, name)
            case _ => Select(t, name.toTypeName)
          }
          while (in.token == DOT) {
            in.nextToken()
            t = typeArgs(atPos(t.pos.start, in.offset)(typeSelect(t, ident())))
          }
          convertToTypeId(t)
        } else {
          basicType()
        }
      }

    def typeArgs(t: Tree): Tree = {
      var wildnum = 0
      def typeArg(): Tree =
        if (in.token == QMARK) {
          val offset = in.offset
          in.nextToken()
          val hi = if (in.token == EXTENDS) { in.nextToken() ; typ() } else EmptyTree
          val lo = if (in.token == SUPER)   { in.nextToken() ; typ() } else EmptyTree
          atPos(offset) {
            /*
            TypeDef(
              Modifiers(Flags.JavaDefined | Flags.Deferred),
              typeName("_$" +(wildnum += 1)),
              List(),
              TypeBoundsTree(lo, hi))
              */
            TypeBoundsTree(lo, hi)
          }
        } else {
          typ()
        }
      if (in.token == LT) {
        in.nextToken()
        val t1 = convertToTypeId(t)
        val args = repsep(() => typeArg(), COMMA)
        acceptClosingAngle()
        atPos(t1.pos.start) {
          AppliedTypeTree(t1, args)
        }
      } else t
    }

    def annotations(): List[Tree] = {
      //var annots = new ListBuffer[Tree]
      while (in.token == AT) {
        in.nextToken()
        annotation()
      }
      List() // don't pass on annotations for now
    }

    /** Annotation ::= TypeName [`(` AnnotationArgument {`,` AnnotationArgument} `)`]
      */
    def annotation(): Unit = {
      qualId()
      if (in.token == LPAREN) { skipAhead(); accept(RPAREN) }
      else if (in.token == LBRACE) { skipAhead(); accept(RBRACE) }
    }

    def modifiers(inInterface: Boolean): Modifiers = {
      var flags = Flags.JavaDefined
      // assumed true unless we see public/private/protected
      var isPackageAccess = true
      var annots: List[Tree] = Nil
      def addAnnot(sym: ClassSymbol) =
        annots :+= atPos(in.offset) {
          in.nextToken()
          New(TypeTree(sym.typeRef))
        }

      while (true) {
        in.token match {
          case AT if (in.lookaheadToken != INTERFACE) =>
            in.nextToken()
            annotation()
          case PUBLIC =>
            isPackageAccess = false
            in.nextToken()
          case PROTECTED =>
            flags |= Flags.Protected
            in.nextToken()
          case PRIVATE =>
            isPackageAccess = false
            flags |= Flags.Private
            in.nextToken()
          case STATIC =>
            flags |= Flags.JavaStatic
            in.nextToken()
          case ABSTRACT =>
            flags |= Flags.Abstract
            in.nextToken()
          case FINAL =>
            flags |= Flags.Final
            in.nextToken()
          case DEFAULT =>
            flags |= Flags.DefaultMethod
            in.nextToken()
          case NATIVE =>
            addAnnot(NativeAnnot)
          case TRANSIENT =>
            addAnnot(TransientAnnot)
          case VOLATILE =>
            addAnnot(VolatileAnnot)
          case SYNCHRONIZED | STRICTFP =>
            in.nextToken()
          case _ =>
            val privateWithin: TypeName =
              if (isPackageAccess && !inInterface) thisPackageName
              else tpnme.EMPTY

            return Modifiers(flags, privateWithin) withAnnotations annots
        }
      }
      assert(false, "should not be here")
      throw new RuntimeException
    }

    def typeParams(flags: FlagSet = Flags.JavaDefined | Flags.Private | Flags.Local | Flags.Param): List[TypeDef] =
      if (in.token == LT) {
        in.nextToken()
        val tparams = repsep(() => typeParam(flags), COMMA)
        acceptClosingAngle()
        tparams
      } else List()

    def typeParam(flags: FlagSet): TypeDef =
      atPos(in.offset) {
        val name = identForType()
        val hi = if (in.token == EXTENDS) { in.nextToken() ; bound() } else EmptyTree
        TypeDef(name, TypeBoundsTree(EmptyTree, hi)).withMods(Modifiers(flags))
      }

    def bound(): Tree =
      atPos(in.offset) {
        val buf = ListBuffer[Tree](typ())
        while (in.token == AMP) {
          in.nextToken()
          buf += typ()
        }
        val ts = buf.toList
        if (ts.tail.isEmpty) ts.head
        else ts.reduce(AndTypeTree(_,_))
      }

    def formalParams(): List[ValDef] = {
      accept(LPAREN)
      val vparams = if (in.token == RPAREN) List() else repsep(() => formalParam(), COMMA)
      accept(RPAREN)
      vparams
    }

    def formalParam(): ValDef = {
      val start = in.offset
      if (in.token == FINAL) in.nextToken()
      annotations()
      var t = typ()
      if (in.token == DOTDOTDOT) {
        in.nextToken()
        t = atPos(t.pos.start) {
          PostfixOp(t, Ident(tpnme.raw.STAR))
        }
      }
      atPos(start, in.offset) {
        varDecl(Modifiers(Flags.JavaDefined | Flags.Param), t, ident().toTermName)
      }
    }

    def optThrows(): Unit = {
      if (in.token == THROWS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      }
    }

    def methodBody(): Tree = atPos(in.offset) {
      skipAhead()
      accept(RBRACE) // skip block
      unimplementedExpr
    }

    def definesInterface(token: Int) = token == INTERFACE || token == AT

    def termDecl(start: Offset, mods: Modifiers, parentToken: Int, parentTParams: List[TypeDef]): List[Tree] = {
      val inInterface = definesInterface(parentToken)
      val tparams = if (in.token == LT) typeParams(Flags.JavaDefined | Flags.Param) else List()
      val isVoid = in.token == VOID
      var rtpt =
        if (isVoid)
          atPos(in.offset) {
            in.nextToken()
            TypeTree(UnitType)
          }
        else typ()
      var nameOffset = in.offset
      val rtptName = rtpt match {
        case Ident(name) => name
        case _ => nme.EMPTY
      }
      if (in.token == LPAREN && rtptName != nme.EMPTY && !inInterface) {
        // constructor declaration
        val vparams = formalParams()
        optThrows()
        List {
          atPos(start) {
            DefDef(nme.CONSTRUCTOR, parentTParams,
                List(vparams), TypeTree(), methodBody()).withMods(mods)
          }
        }
      } else {
        var mods1 = mods
        if (mods is Flags.Abstract) mods1 = mods &~ Flags.Abstract
        nameOffset = in.offset
        val name = ident()
        if (in.token == LPAREN) {
          // method declaration
          val vparams = formalParams()
          if (!isVoid) rtpt = optArrayBrackets(rtpt)
          optThrows()
          val bodyOk = !inInterface || (mods is Flags.DefaultMethod)
          val body =
            if (bodyOk && in.token == LBRACE) {
              methodBody()
            } else {
              if (parentToken == AT && in.token == DEFAULT) {
                val annot =
                  atPos(nameOffset) {
                    New(Select(scalaDot(nme.runtime), tpnme.AnnotationDefaultATTR), Nil)
                  }
                mods1 = mods1 withAddedAnnotation annot
                val unimplemented = unimplementedExpr
                skipTo(SEMI)
                accept(SEMI)
                unimplemented
              } else {
                accept(SEMI)
                EmptyTree
              }
            }
          //if (inInterface) mods1 |= Flags.Deferred
          List {
            atPos(start, nameOffset) {
              DefDef(name.toTermName, tparams, List(vparams), rtpt, body).withMods(mods1 | Flags.Method)
            }
          }
        } else {
          if (inInterface) mods1 |= Flags.Final | Flags.JavaStatic
          val result = fieldDecls(start, nameOffset, mods1, rtpt, name)
          accept(SEMI)
          result
        }
      }
    }

    /** Parse a sequence of field declarations, separated by commas.
      *  This one is tricky because a comma might also appear in an
      *  initializer. Since we don't parse initializers we don't know
      *  what the comma signifies.
      *  We solve this with a second list buffer `maybe` which contains
      *  potential variable definitions.
      *  Once we have reached the end of the statement, we know whether
      *  these potential definitions are real or not.
      */
    def fieldDecls(start: Offset, firstNameOffset: Offset, mods: Modifiers, tpt: Tree, name: Name): List[Tree] = {
      val buf = ListBuffer[Tree](
          atPos(start, firstNameOffset) { varDecl(mods, tpt, name.toTermName) })
      val maybe = new ListBuffer[Tree] // potential variable definitions.
      while (in.token == COMMA) {
        in.nextToken()
        if (in.token == IDENTIFIER) { // if there's an ident after the comma ...
        val nextNameOffset = in.offset
        val name = ident()
          if (in.token == EQUALS || in.token == SEMI) { // ... followed by a `=` or `;`, we know it's a real variable definition
            buf ++= maybe
            buf += atPos(start, nextNameOffset) { varDecl(mods, tpt, name.toTermName) }
            maybe.clear()
          } else if (in.token == COMMA) { // ... if there's a comma after the ident, it could be a real vardef or not.
            maybe += atPos(start, nextNameOffset) { varDecl(mods, tpt, name.toTermName) }
          } else { // ... if there's something else we were still in the initializer of the
            // previous var def; skip to next comma or semicolon.
            skipTo(COMMA, SEMI)
            maybe.clear()
          }
        } else { // ... if there's no ident following the comma we were still in the initializer of the
          // previous var def; skip to next comma or semicolon.
          skipTo(COMMA, SEMI)
          maybe.clear()
        }
      }
      if (in.token == SEMI) {
        buf ++= maybe // every potential vardef that survived until here is real.
      }
      buf.toList
    }

    def varDecl(mods: Modifiers, tpt: Tree, name: TermName): ValDef = {
      val tpt1 = optArrayBrackets(tpt)
      if (in.token == EQUALS && !(mods is Flags.Param)) skipTo(COMMA, SEMI)
      val mods1 = if (mods is Flags.Final) mods else mods | Flags.Mutable
      ValDef(name, tpt1, if (mods is Flags.Param) EmptyTree else unimplementedExpr).withMods(mods1)
    }

    def memberDecl(start: Offset, mods: Modifiers, parentToken: Int, parentTParams: List[TypeDef]): List[Tree] = in.token match {
      case CLASS | ENUM | INTERFACE | AT =>
        typeDecl(start, if (definesInterface(parentToken)) mods | Flags.JavaStatic else mods)
      case _ =>
        termDecl(start, mods, parentToken, parentTParams)
    }

    def makeCompanionObject(cdef: TypeDef, statics: List[Tree]): Tree =
      atPos(cdef.pos) {
        assert(cdef.pos.exists)
        ModuleDef(cdef.name.toTermName,
          makeTemplate(List(), statics, List(), false)).withMods((cdef.mods & Flags.RetainedModuleClassFlags).toTermFlags)
      }

    def importCompanionObject(cdef: TypeDef): Tree =
      Import(Ident(cdef.name.toTermName).withPos(NoPosition), Ident(nme.WILDCARD) :: Nil)

    // Importing the companion object members cannot be done uncritically: see
    // ticket #2377 wherein a class contains two static inner classes, each of which
    // has a static inner class called "Builder" - this results in an ambiguity error
    // when each performs the import in the enclosing class's scope.
    //
    // To address this I moved the import Companion._ inside the class, as the first
    // statement.  This should work without compromising the enclosing scope, but may (?)
    // end up suffering from the same issues it does in scala - specifically that this
    // leaves auxiliary constructors unable to access members of the companion object
    // as unqualified identifiers.
    def addCompanionObject(statics: List[Tree], cdef: TypeDef): List[Tree] = {
      // if there are no statics we can use the original cdef, but we always
      // create the companion so import A._ is not an error (see ticket #1700)
      val cdefNew =
        if (statics.isEmpty) cdef
        else {
          val template = cdef.rhs.asInstanceOf[Template]
          cpy.TypeDef(cdef)(cdef.name,
            cpy.Template(template)(template.constr, template.parents, template.self,
              importCompanionObject(cdef) :: template.body)).withMods(cdef.mods)
        }

      List(makeCompanionObject(cdefNew, statics), cdefNew)
    }

    def importDecl(): List[Tree] = {
      val start = in.offset
      accept(IMPORT)
      val buf = new ListBuffer[Name]
      def collectIdents() : Int = {
        if (in.token == ASTERISK) {
          val starOffset = in.offset
          in.nextToken()
          buf += nme.WILDCARD
          starOffset
        } else {
          val nameOffset = in.offset
          buf += ident()
          if (in.token == DOT) {
            in.nextToken()
            collectIdents()
          } else nameOffset
        }
      }
      if (in.token == STATIC) in.nextToken()
      else buf += nme.ROOTPKG
      val lastnameOffset = collectIdents()
      accept(SEMI)
      val names = buf.toList
      if (names.length < 2) {
        syntaxError(start, "illegal import", skipIt = false)
        List()
      } else {
        val qual = ((Ident(names.head): Tree) /: names.tail.init) (Select(_, _))
        val lastname = names.last
        val ident = Ident(lastname) withPos Position(lastnameOffset)
//        val selector = lastname match {
//          case nme.WILDCARD => Pair(ident, Ident(null) withPos Position(-1))
//          case _            => Pair(ident, ident)
//        }
        val imp = atPos(start) { Import(qual, List(ident)) }
        imp :: Nil
      }
    }

    def interfacesOpt() =
      if (in.token == IMPLEMENTS) {
        in.nextToken()
        repsep(() => typ(), COMMA)
      } else {
        List()
      }

    def classDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(CLASS)
      val nameOffset = in.offset
      val name = identForType()
      val tparams = typeParams()
      val superclass =
        if (in.token == EXTENDS) {
          in.nextToken()
          typ()
        } else {
          javaLangObject()
        }
      val interfaces = interfacesOpt()
      val (statics, body) = typeBody(CLASS, name, tparams)
      val cls = atPos(start, nameOffset) {
        TypeDef(name, makeTemplate(superclass :: interfaces, body, tparams, true)).withMods(mods)
      }
      addCompanionObject(statics, cls)
    }

    def interfaceDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(INTERFACE)
      val nameOffset = in.offset
      val name = identForType()
      val tparams = typeParams()
      val parents =
        if (in.token == EXTENDS) {
          in.nextToken()
          repsep(() => typ(), COMMA)
        } else {
          List(javaLangObject())
        }
      val (statics, body) = typeBody(INTERFACE, name, tparams)
      val iface = atPos(start, nameOffset) {
        TypeDef(
          name,
          makeTemplate(parents, body, tparams, false)).withMods(mods | Flags.Trait | Flags.JavaDefined | Flags.Abstract)
      }
      addCompanionObject(statics, iface)
    }

    def typeBody(leadingToken: Int, parentName: Name, parentTParams: List[TypeDef]): (List[Tree], List[Tree]) = {
      accept(LBRACE)
      val defs = typeBodyDecls(leadingToken, parentName, parentTParams)
      accept(RBRACE)
      defs
    }

    def typeBodyDecls(parentToken: Int, parentName: Name, parentTParams: List[TypeDef]): (List[Tree], List[Tree]) = {
      val inInterface = definesInterface(parentToken)
      val statics = new ListBuffer[Tree]
      val members = new ListBuffer[Tree]
      while (in.token != RBRACE && in.token != EOF) {
        val start = in.offset
        var mods = atPos(start) { modifiers(inInterface) }
        if (in.token == LBRACE) {
          skipAhead() // skip init block, we just assume we have seen only static
          accept(RBRACE)
        } else if (in.token == SEMI) {
          in.nextToken()
        } else {
          if (in.token == ENUM || definesInterface(in.token)) mods |= Flags.JavaStatic
          val decls = memberDecl(start, mods, parentToken, parentTParams)
          (if ((mods is Flags.JavaStatic) || inInterface && !(decls exists (_.isInstanceOf[DefDef])))
            statics
          else
            members) ++= decls
        }
      }
      def forwarders(sdef: Tree): List[Tree] = sdef match {
        case TypeDef(name, _) if (parentToken == INTERFACE) =>
          var rhs: Tree = Select(Ident(parentName.toTermName), name)
          List(TypeDef(name, rhs).withMods(Modifiers(Flags.Protected)))
        case _ =>
          List()
      }
      val sdefs = statics.toList
      val idefs = members.toList ::: (sdefs flatMap forwarders)
      (sdefs, idefs)
    }
    def annotationParents = List(
      scalaAnnotationDot(tpnme.Annotation),
      Select(javaLangDot(nme.annotation), tpnme.Annotation),
      scalaAnnotationDot(tpnme.ClassfileAnnotation)
    )
    def annotationDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(AT)
      accept(INTERFACE)
      val nameOffset = in.offset
      val name = identForType()
      val (statics, body) = typeBody(AT, name, List())
      val constructorParams = body.collect {
        case dd: DefDef => makeParam(dd.name, dd.tpt)
      }
      val constr = DefDef(nme.CONSTRUCTOR,
        List(), List(constructorParams), TypeTree(), EmptyTree).withMods(Modifiers(Flags.JavaDefined))
      val body1 = body.filterNot(_.isInstanceOf[DefDef])
      val templ = makeTemplate(annotationParents, constr :: body1, List(), false)
      val annot = atPos(start, nameOffset) {
        TypeDef(name, templ).withMods(mods | Flags.Abstract)
      }
      addCompanionObject(statics, annot)
    }

    def enumDecl(start: Offset, mods: Modifiers): List[Tree] = {
      accept(ENUM)
      val nameOffset = in.offset
      val name = identForType()
      def enumType = Ident(name)
      val interfaces = interfacesOpt()
      accept(LBRACE)
      val buf = new ListBuffer[Tree]
      def parseEnumConsts(): Unit = {
        if (in.token != RBRACE && in.token != SEMI && in.token != EOF) {
          buf += enumConst(enumType)
          if (in.token == COMMA) {
            in.nextToken()
            parseEnumConsts()
          }
        }
      }
      parseEnumConsts()
      val consts = buf.toList
      val (statics, body) =
        if (in.token == SEMI) {
          in.nextToken()
          typeBodyDecls(ENUM, name, List())
        } else {
          (List(), List())
        }
      val predefs = List(
        DefDef(
          nme.values, List(),
          ListOfNil,
          arrayOf(enumType),
          unimplementedExpr).withMods(Modifiers(Flags.JavaDefined | Flags.JavaStatic | Flags.Method)),
        DefDef(
          nme.valueOf, List(),
          List(List(makeParam("x".toTermName, TypeTree(StringType)))),
          enumType,
          unimplementedExpr).withMods(Modifiers(Flags.JavaDefined | Flags.JavaStatic | Flags.Method)))
      accept(RBRACE)
      /*
      val superclazz =
        AppliedTypeTree(javaLangDot(tpnme.Enum), List(enumType))
        */
      val superclazz = Apply(TypeApply(
        Select(New(javaLangDot(tpnme.Enum)), nme.CONSTRUCTOR), List(enumType)),
        List(Literal(Constant(null)),Literal(Constant(0))))
      val enumclazz = atPos(start, nameOffset) {
        TypeDef(name,
          makeTemplate(superclazz :: interfaces, body, List(), true)).withMods(mods | Flags.Enum)
      }
      addCompanionObject(consts ::: statics ::: predefs, enumclazz)
    }

    def enumConst(enumType: Tree) = {
      annotations()
      atPos(in.offset) {
        val name = ident()
        if (in.token == LPAREN) {
          // skip arguments
          skipAhead()
          accept(RPAREN)
        }
        if (in.token == LBRACE) {
          // skip classbody
          skipAhead()
          accept(RBRACE)
        }
        ValDef(name.toTermName, enumType, unimplementedExpr).withMods(Modifiers(Flags.Enum | Flags.Stable | Flags.JavaDefined | Flags.JavaStatic))
      }
    }

    def typeDecl(start: Offset, mods: Modifiers): List[Tree] = in.token match {
      case ENUM      => enumDecl(start, mods)
      case INTERFACE => interfaceDecl(start, mods)
      case AT        => annotationDecl(start, mods)
      case CLASS     => classDecl(start, mods)
      case _         => in.nextToken(); syntaxError("illegal start of type declaration", skipIt = true); List(errorTypeTree)
    }

    /** CompilationUnit ::= [package QualId semi] TopStatSeq
      */
    def compilationUnit(): Tree = {
      val start = in.offset
      val pkg: RefTree =
        if (in.token == AT || in.token == PACKAGE) {
          annotations()
          accept(PACKAGE)
          val pkg = qualId()
          accept(SEMI)
          pkg
        } else {
          Ident(nme.EMPTY_PACKAGE)
        }
      thisPackageName = convertToTypeName(pkg) match {
        case Some(t)  => t.name.toTypeName
        case _        => tpnme.EMPTY
      }
      val buf = new ListBuffer[Tree]
      while (in.token == IMPORT)
        buf ++= importDecl()
      while (in.token != EOF && in.token != RBRACE) {
        while (in.token == SEMI) in.nextToken()
        if (in.token != EOF) {
          val start = in.offset
          val mods = atPos(start) { modifiers(inInterface = false) }
          buf ++= typeDecl(start, mods)
        }
      }
      val unit = atPos(start) { PackageDef(pkg, buf.toList) }
      accept(EOF)
      unit
    }
  }
}
