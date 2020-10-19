package dotty.tools
package dotc
package core
package classfile

import dotty.tools.tasty.{ TastyReader, TastyHeaderUnpickler }

import Contexts._, Symbols._, Types._, Names._, StdNames._, NameOps._, Scopes._, Decorators._
import SymDenotations._, unpickleScala2.Scala2Unpickler._, Constants._, Annotations._, util.Spans._
import Phases._
import NameKinds.DefaultGetterName
import ast.{ tpd, untpd }
import ast.tpd._, util._
import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, IOException }

import java.lang.Integer.toHexString
import java.net.URLClassLoader
import java.util.UUID

import scala.collection.immutable
import scala.collection.mutable.{ ListBuffer, ArrayBuffer }
import scala.annotation.switch
import typer.Checking.checkNonCyclic
import io.{AbstractFile, PlainFile, ZipArchive}
import scala.util.control.NonFatal
import util.Lst
import util.Lst.{NIL, +:, toLst}

object ClassfileParser {
  /** Marker trait for unpicklers that can be embedded in classfiles. */
  trait Embedded

  /** Indicate that there is nothing to unpickle and the corresponding symbols can
    * be invalidated. */
  object NoEmbedded extends Embedded

  /** Replace raw types with wildcard applications */
  def cook(using Context): TypeMap = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: TypeRef if tp.symbol.typeParams.nonEmpty =>
        AppliedType(tp, tp.symbol.typeParams.map(Function.const(TypeBounds.empty)))
      case tp @ AppliedType(tycon, args) =>
        // disregard tycon itself, but map over it to visit the prefix
        tp.derivedAppliedType(mapOver(tycon), args.mapConserve(this))
      case tp @ TempPolyType(_, tpe) =>
        val tpe1 = this(tpe)
        if (tpe1 eq tpe) tp else tp.copy(tpe = tpe1)
      case tp @ TempClassInfoType(parents, _, _) =>
        val parents1 = parents.mapConserve(this)
        if (parents eq parents1) tp else tp.copy(parentTypes = parents1)
      case _ =>
        mapOver(tp)
    }
  }
}

class ClassfileParser(
    classfile: AbstractFile,
    classRoot: ClassDenotation,
    moduleRoot: ClassDenotation)(ictx: Context) {

  //println(s"parsing ${classRoot.name.debugString} ${moduleRoot.name.debugString}")

  import ClassfileConstants._
  import ClassfileParser._

  protected val in: AbstractFileReader = new AbstractFileReader(classfile)

  protected val staticModule: Symbol = moduleRoot.sourceModule(using ictx)

  protected val instanceScope: MutableScope = newScope     // the scope of all instance definitions
  protected val staticScope: MutableScope = newScope       // the scope of all static definitions
  protected var pool: ConstantPool = _              // the classfile's constant pool

  protected var currentClassName: SimpleName = _      // JVM name of the current class
  protected var classTParams: Map[Name, Symbol] = Map()

  private var Scala2UnpicklingMode = Mode.Scala2Unpickling

  classRoot.info = NoLoader().withDecls(instanceScope)
  moduleRoot.info = NoLoader().withDecls(staticScope).withSourceModule(staticModule)

  private def currentIsTopLevel(using Context) = classRoot.owner.is(Flags.PackageClass)

  private def mismatchError(className: SimpleName) =
    throw new IOException(s"class file '${in.file.canonicalPath}' has location not matching its contents: contains class $className")

  def run()(using Context): Option[Embedded] = try {
    report.debuglog("[class] >> " + classRoot.fullName)
    parseHeader()
    this.pool = new ConstantPool
    parseClass()
  }
  catch {
    case e: RuntimeException =>
      if (ctx.debug) e.printStackTrace()
      throw new IOException(
        i"""class file ${classfile.canonicalPath} is broken, reading aborted with ${e.getClass}
           |${Option(e.getMessage).getOrElse("")}""")
  }

  private def parseHeader(): Unit = {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException(s"class file '${in.file}' has wrong magic number 0x${toHexString(magic)}, should be 0x${toHexString(JAVA_MAGIC)}")
    val minorVersion = in.nextChar.toInt
    val majorVersion = in.nextChar.toInt
    if ((majorVersion < JAVA_MAJOR_VERSION) ||
        ((majorVersion == JAVA_MAJOR_VERSION) &&
         (minorVersion < JAVA_MINOR_VERSION)))
      throw new IOException(
        s"class file '${in.file}' has unknown version $majorVersion.$minorVersion, should be at least $JAVA_MAJOR_VERSION.$JAVA_MINOR_VERSION")
  }

  /** Return the class symbol of the given name. */
  def classNameToSymbol(name: Name)(using Context): Symbol = innerClasses.get(name) match {
    case Some(entry) => innerClasses.classSymbol(entry)
    case None => requiredClass(name)
  }

  var sawPrivateConstructor: Boolean = false

  def parseClass()(using Context): Option[Embedded] = {
    val jflags       = in.nextChar
    val isAnnotation = hasAnnotation(jflags)
    val sflags       = classTranslation.flags(jflags)
    val isEnum       = (jflags & JAVA_ACC_ENUM) != 0
    val nameIdx      = in.nextChar
    currentClassName = pool.getClassName(nameIdx)

    if (currentIsTopLevel &&
        currentClassName != classRoot.fullName.toSimpleName &&
        currentClassName != classRoot.fullName.encode.toSimpleName)
      mismatchError(currentClassName)

    addEnclosingTParams()

    /** Parse parents for Java classes. For Scala, return AnyRef, since the real type will be unpickled.
     *  Updates the read pointer of 'in'. */
    def parseParents: List[Type] = {
      val superType =
        if (isAnnotation) {
          in.nextChar
          defn.AnnotationClass.typeRef
        }
        else if (classRoot.symbol == defn.ComparableClass ||
                 classRoot.symbol == defn.JavaCloneableClass ||
                 classRoot.symbol == defn.JavaSerializableClass) {
          // Treat these interfaces as universal traits
          in.nextChar
          defn.AnyType
        }
        else
          pool.getSuperClass(in.nextChar).typeRef
      val ifaceCount = in.nextChar
      var ifaces = for (i <- (0 until ifaceCount).toList) yield pool.getSuperClass(in.nextChar).typeRef
        // Dotty deviation: was
        //    var ifaces = for (i <- List.range(0, ifaceCount)) ...
        // This does not typecheck because the type parameter of List is now lower-bounded by Int | Char.
        // Consequently, no best implicit for the "Integral" evidence parameter of "range"
        // is found. Previously, this worked because of weak conformance, which has been dropped.

      if (isAnnotation) ifaces = defn.ClassfileAnnotationClass.typeRef :: ifaces
      superType :: ifaces
    }

    val result = unpickleOrParseInnerClasses()
    if (!result.isDefined) {
      var classInfo: Type = TempClassInfoType(parseParents, instanceScope, classRoot.symbol)
      // might be reassigned by later parseAttributes
      val staticInfo = TempClassInfoType(List(), staticScope, moduleRoot.symbol)

      enterOwnInnerClasses()

      classRoot.setFlag(sflags)
      moduleRoot.setFlag(Flags.JavaDefined | Flags.ModuleClassCreationFlags)

      val privateWithin = getPrivateWithin(jflags)

      classRoot.setPrivateWithin(privateWithin)
      moduleRoot.setPrivateWithin(privateWithin)
      moduleRoot.sourceModule.setPrivateWithin(privateWithin)

      for (i <- 0 until in.nextChar) parseMember(method = false)
      for (i <- 0 until in.nextChar) parseMember(method = true)

      classRoot.registerCompanion(moduleRoot.symbol)
      moduleRoot.registerCompanion(classRoot.symbol)

      setClassInfo(moduleRoot, staticInfo, fromScala2 = false)

      classInfo = parseAttributes(classRoot.symbol, classInfo)
      if (isAnnotation)
        // classInfo must be a TempClassInfoType and not a TempPolyType,
        // because Java annotations cannot have type parameters.
        addAnnotationConstructor(classInfo.asInstanceOf[TempClassInfoType])

      setClassInfo(classRoot, classInfo, fromScala2 = false)
    }
    else if (result == Some(NoEmbedded))
      for (sym <- List(moduleRoot.sourceModule, moduleRoot.symbol, classRoot.symbol)) {
        classRoot.owner.asClass.delete(sym)
        if (classRoot.owner == defn.ScalaShadowingPackage.moduleClass)
          // Symbols in scalaShadowing are also added to scala
          defn.ScalaPackageClass.delete(sym)
        sym.markAbsent()
      }

    result
  }

  /** Add type parameters of enclosing classes */
  def addEnclosingTParams()(using Context): Unit = {
    var sym = classRoot.owner
    while (sym.isClass && !sym.is(Flags.ModuleClass)) {
      for (tparam <- sym.typeParams)
        classTParams = classTParams.updated(tparam.name, tparam)
      sym = sym.owner
    }
  }

  def parseMember(method: Boolean)(using Context): Unit = {
    val start = indexCoord(in.bp)
    val jflags = in.nextChar
    val sflags =
      if (method) Flags.Method | methodTranslation.flags(jflags)
      else fieldTranslation.flags(jflags)
    val name = pool.getName(in.nextChar)
    if (!sflags.isOneOf(Flags.PrivateOrArtifact) || name == nme.CONSTRUCTOR) {
      val member = newSymbol(
        getOwner(jflags), name, sflags, memberCompleter,
        getPrivateWithin(jflags), coord = start)
      getScope(jflags).enter(member)
    }
    // skip rest of member for now
    in.nextChar // info
    skipAttributes()
  }

  val memberCompleter: LazyType = new LazyType {

    def complete(denot: SymDenotation)(using Context): Unit = {
      val oldbp = in.bp
      try {
        in.bp = denot.symbol.coord.toIndex
        val sym = denot.symbol
        val jflags = in.nextChar
        val isEnum = (jflags & JAVA_ACC_ENUM) != 0
        val name = pool.getName(in.nextChar)
        val isConstructor = name eq nme.CONSTRUCTOR

        /** Strip leading outer param from constructor and trailing access tag for
         *  private inner constructors.
         */
        def normalizeConstructorParams() = innerClasses.get(currentClassName) match {
          case Some(entry) if !isStatic(entry.jflags) =>
            val mt @ MethodTpe(paramNames, paramTypes, resultType) = denot.info
            var normalizedParamNames = paramNames.tail
            var normalizedParamTypes = paramTypes.tail
            if ((jflags & JAVA_ACC_SYNTHETIC) != 0) {
              // SI-7455 strip trailing dummy argument ("access constructor tag") from synthetic constructors which
              // are added when an inner class needs to access a private constructor.
              normalizedParamNames = paramNames.dropRight(1)
              normalizedParamTypes = paramTypes.dropRight(1)
            }
            denot.info = mt.derivedLambdaType(normalizedParamNames, normalizedParamTypes, resultType)
          case _ =>
        }

        /** Make return type of constructor be the enclosing class type,
         *  and make constructor type polymorphic in the type parameters of the class
         */
        def normalizeConstructorInfo() = {
          val rt = classRoot.typeRef appliedTo (classRoot.typeParams map (_.typeRef))

          def resultType(tpe: Type): Type = tpe match {
            case mt @ MethodType(paramNames) => mt.derivedLambdaType(paramNames, mt.paramInfos, rt)
            case pt : PolyType => pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType))
          }

          denot.info = resultType(denot.info)
          addConstructorTypeParams(denot)
        }

        val isVarargs = denot.is(Flags.Method) && (jflags & JAVA_ACC_VARARGS) != 0
        denot.info = pool.getType(in.nextChar, isVarargs)
        if (isConstructor) normalizeConstructorParams()
        denot.info = translateTempPoly(parseAttributes(sym, denot.info, isVarargs))
        if (isConstructor) normalizeConstructorInfo()

        if (ctx.explicitNulls) denot.info = JavaNullInterop.nullifyMember(denot.symbol, denot.info, isEnum)

        // seal java enums
        if (isEnum) {
          val enumClass = sym.owner.linkedClass
          if (!enumClass.exists)
            report.warning(s"no linked class for java enum $sym in ${sym.owner}. A referencing class file might be missing an InnerClasses entry.")
          else {
            if (!enumClass.is(Flags.Sealed)) enumClass.setFlag(Flags.AbstractSealed)
            enumClass.addAnnotation(Annotation.Child(sym, NoSpan))
          }
        }
      }
      finally
        in.bp = oldbp
    }
  }

  def constantTagToType(tag: Int)(using Context): Type =
    (tag: @switch) match {
      case BYTE_TAG   => defn.ByteType
      case CHAR_TAG   => defn.CharType
      case DOUBLE_TAG => defn.DoubleType
      case FLOAT_TAG  => defn.FloatType
      case INT_TAG    => defn.IntType
      case LONG_TAG   => defn.LongType
      case SHORT_TAG  => defn.ShortType
      case VOID_TAG   => defn.UnitType
      case BOOL_TAG   => defn.BooleanType
    }

  private def sigToType(sig: SimpleName, owner: Symbol = null, isVarargs: Boolean = false)(using Context): Type = {
    var index = 0
    val end = sig.length
    def accept(ch: Char): Unit = {
      assert(sig(index) == ch, (sig(index), ch))
      index += 1
    }
    def subName(isDelimiter: Char => Boolean): SimpleName = {
      val start = index
      while (!isDelimiter(sig(index))) { index += 1 }
      sig.slice(start, index)
    }
    // Warning: sigToType contains nested completers which might be forced in a later run!
    // So local methods need their own ctx parameters.
    def sig2type(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean)(using Context): Type = {
      val tag = sig(index); index += 1
      (tag: @switch) match {
        case 'L' =>
          def processInner(tp: Type): Type = tp match {
            case tp: TypeRef if !tp.symbol.owner.is(Flags.ModuleClass) =>
              TypeRef(processInner(tp.prefix.widen), tp.symbol.asType)
            case _ =>
              tp
          }
          def processClassType(tp: Type): Type = tp match {
            case tp: TypeRef =>
              if (sig(index) == '<') {
                accept('<')
                val argsBuf = if (skiptvs) null else new ListBuffer[Type]
                while (sig(index) != '>') {
                  val arg = sig(index) match {
                    case variance @ ('+' | '-' | '*') =>
                      index += 1
                      variance match {
                        case '+' => TypeBounds.upper(sig2type(tparams, skiptvs))
                        case '-' =>
                          val argTp = sig2type(tparams, skiptvs)
                          // Interpret `sig2type` returning `Any` as "no bounds";
                          // morally equivalent to TypeBounds.empty, but we're representing Java code, so use FromJavaObjectType as the upper bound
                          if (argTp.typeSymbol == defn.AnyClass) TypeBounds.upper(defn.FromJavaObjectType)
                          else TypeBounds(argTp, defn.FromJavaObjectType)
                        case '*' => TypeBounds.upper(defn.FromJavaObjectType)
                      }
                    case _ => sig2type(tparams, skiptvs)
                  }
                  if (argsBuf != null) argsBuf += arg
                }
                accept('>')
                if (skiptvs) tp else tp.appliedTo(argsBuf.toList)
              }
              else tp
            case tp =>
              assert(sig(index) != '<', tp)
              tp
          }

          val classSym = classNameToSymbol(subName(c => c == ';' || c == '<'))
          val classTpe = if (classSym eq defn.ObjectClass) defn.FromJavaObjectType else classSym.typeRef
          var tpe = processClassType(processInner(classTpe))
          while (sig(index) == '.') {
            accept('.')
            val name = subName(c => c == ';' || c == '<' || c == '.').toTypeName
            val clazz = tpe.member(name).symbol
            tpe = processClassType(processInner(TypeRef(tpe, clazz)))
          }
          accept(';')
          tpe
        case ARRAY_TAG =>
          while ('0' <= sig(index) && sig(index) <= '9') index += 1
          val elemtp = sig2type(tparams, skiptvs)
          defn.ArrayOf(elemtp.translateJavaArrayElementType)
        case '(' =>
          def isMethodEnd(i: Int) = sig(i) == ')'
          def isArray(i: Int) = sig(i) == '['

          /** Is this a repeated parameter type?
           *  This is true if we're in a vararg method and this is the last parameter.
           */
          def isRepeatedParam(i: Int): Boolean =
            if !isVarargs then return false
            var cur = i
            // Repeated parameters are represented as arrays
            if !isArray(cur) then return false
            // Handle nested arrays: int[]...
            while isArray(cur) do
              cur += 1
            // Simple check to see if we're the last parameter: there should be no
            // array in the signature until the method end.
            while !isMethodEnd(cur) do
              if isArray(cur) then return false
              cur += 1
            true
          end isRepeatedParam

          val paramtypes = new ListBuffer[Type]()
          var paramnames = new ListBuffer[TermName]()
          while !isMethodEnd(index) do
            paramnames += nme.syntheticParamName(paramtypes.length)
            paramtypes += {
              if isRepeatedParam(index) then
                index += 1
                val elemType = sig2type(tparams, skiptvs)
                // `ElimRepeated` is responsible for correctly erasing this.
                defn.RepeatedParamType.appliedTo(elemType)
              else
                sig2type(tparams, skiptvs)
            }

          index += 1
          val restype = sig2type(tparams, skiptvs)
          JavaMethodType(paramnames.toList, paramtypes.toList, restype)
        case 'T' =>
          val n = subName(';'.==).toTypeName
          index += 1
          //assert(tparams contains n, s"classTparams = $classTParams, tparams = $tparams, key = $n")
          if (skiptvs) defn.AnyType else tparams(n).typeRef
        case tag =>
          constantTagToType(tag)
      }
    }
    // sig2type(tparams, skiptvs)

    def sig2typeBounds(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean)(using Context): Type = {
      val ts = new ListBuffer[Type]
      while (sig(index) == ':') {
        index += 1
        if (sig(index) != ':') // guard against empty class bound
          ts += sig2type(tparams, skiptvs)
      }
      val bound = if ts.isEmpty then defn.AnyType else ts.reduceLeft(AndType.apply)
      TypeBounds.upper(bound)
    }

    var tparams = classTParams

    def typeParamCompleter(start: Int) = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val savedIndex = index
        try {
          index = start
          denot.info =
            checkNonCyclic( // we need the checkNonCyclic call to insert LazyRefs for F-bounded cycles
                denot.symbol,
                sig2typeBounds(tparams, skiptvs = false),
                reportErrors = false)
        }
        finally
          index = savedIndex
      }
    }

    val newTParams = new ListBuffer[Symbol]()
    if (sig(index) == '<') {
      assert(owner != null)
      index += 1
      val start = index
      while (sig(index) != '>') {
        val tpname = subName(':'.==).toTypeName
        val s = newSymbol(
          owner, tpname, owner.typeParamCreationFlags,
          typeParamCompleter(index), coord = indexCoord(index))
        if (owner.isClass) owner.asClass.enter(s)
        tparams = tparams + (tpname -> s)
        sig2typeBounds(tparams, skiptvs = true)
        newTParams += s
      }
      index += 1
    }
    val ownTypeParams = newTParams.toList.asInstanceOf[List[TypeSymbol]]
    val tpe =
      if ((owner == null) || !owner.isClass)
        sig2type(tparams, skiptvs = false)
      else {
        classTParams = tparams
        val parents = new ListBuffer[Type]()
        while (index < end)
          parents += sig2type(tparams, skiptvs = false) // here the variance doesn't matter
        TempClassInfoType(parents.toList, instanceScope, owner)
      }
    if (ownTypeParams.isEmpty) tpe else TempPolyType(ownTypeParams, tpe)
  }
  // sigToType

  def parseAnnotArg(skip: Boolean = false)(using Context): Option[untpd.Tree] = {

    // If we encounter an empty array literal, we need the type of the corresponding
    // parameter to properly type it, but that would require forcing the annotation
    // early. To avoid this we store annotation arguments as untyped trees
    import untpd._

    // ... but constants need to actually be typed with a ConstantType, so we
    // can't rely on type inference, and type them early.
    def lit(c: Constant): Tree = TypedSplice(ast.tpd.Literal(c))

    val tag = in.nextByte.toChar
    val index = in.nextChar

    tag match {
      case STRING_TAG =>
        if (skip) None else Some(lit(Constant(pool.getName(index).toString)))
      case BOOL_TAG | BYTE_TAG | CHAR_TAG | SHORT_TAG =>
        if (skip) None else Some(lit(pool.getConstant(index, constantTagToType(tag))))
      case INT_TAG | LONG_TAG | FLOAT_TAG | DOUBLE_TAG =>
        if (skip) None else Some(lit(pool.getConstant(index)))
      case CLASS_TAG =>
        if (skip) None else Some(lit(Constant(pool.getType(index))))
      case ENUM_TAG =>
        val enumClassTp = pool.getType(index)
        val enumCaseName = pool.getName(in.nextChar)
        if (skip)
          None
        else {
          val enumModuleClass = enumClassTp.classSymbol.companionModule
          Some(Select(ref(enumModuleClass), enumCaseName))
        }
      case ARRAY_TAG =>
        val arr = Lst.Buffer[Tree]()
        var hasError = false
        for (i <- 0 until index)
          parseAnnotArg(skip) match {
            case Some(c) => arr += c
            case None => hasError = true
          }
        if (hasError) None
        else if (skip) None
        else {
          val elems = arr.toLst
          Some(untpd.JavaSeqLiteral(elems, TypeTree()))
        }
      case ANNOTATION_TAG =>
        parseAnnotation(index, skip).map(_.untpdTree)
    }
  }

  class ClassfileAnnotation(annotType: Type, args: Lst[untpd.Tree]) extends LazyAnnotation {
    protected var mySym: Symbol | (Context ?=> Symbol) =
      (using ctx: Context) => annotType.classSymbol

    protected var myTree: Tree | (Context ?=> Tree) =
      (using ctx: Context) => untpd.resolveConstructor(annotType, args)

    def untpdTree(using Context): untpd.Tree =
      untpd.New(untpd.TypeTree(annotType), Lst(args))
  }

  /** Parse and return a single annotation.  If it is malformed,
   *  return None.
   */
  def parseAnnotation(attrNameIndex: Char, skip: Boolean = false)(using Context): Option[ClassfileAnnotation] = try {
    val attrType = pool.getType(attrNameIndex)
    attrType match
      case tp: TypeRef =>
        // Silently ignore missing annotation classes like javac
        if tp.denot.infoOrCompleter.isInstanceOf[StubInfo] then
          if ctx.debug then
            report.warning(i"Error while parsing annotations in ${in.file}: annotation class $tp not present on classpath")
          return None
      case _ =>

    val nargs = in.nextChar
    val argbuf = Lst.Buffer[untpd.Tree]()
    var hasError = false
    for (i <- 0 until nargs) {
      val name = pool.getName(in.nextChar)
      parseAnnotArg(skip) match {
        case Some(arg) => argbuf += untpd.NamedArg(name, arg)
        case None => hasError = !skip
      }
    }
    if (hasError || skip) None
    else Some(ClassfileAnnotation(attrType, argbuf.toLst))
  }
  catch {
    case f: FatalError => throw f // don't eat fatal errors, they mean a class was not found
    case NonFatal(ex) =>
      // We want to be robust when annotations are unavailable, so the very least
      // we can do is warn the user about the exception
      // There was a reference to ticket 1135, but that is outdated: a reference to a class not on
      // the classpath would *not* end up here. A class not found is signaled
      // with a `FatalError` exception, handled above. Here you'd end up after a NPE (for example),
      // and that should never be swallowed silently.
      report.warning("Caught: " + ex + " while parsing annotations in " + in.file)
      if (ctx.debug) ex.printStackTrace()

      None // ignore malformed annotations
  }

  def parseAttributes(sym: Symbol, symtype: Type, isVarargs: Boolean = false)(using Context): Type = {
    var newType = symtype

    def parseAttribute(): Unit = {
      val attrName = pool.getName(in.nextChar).toTypeName
      val attrLen = in.nextInt
      val end = in.bp + attrLen
      attrName match {
        case tpnme.SignatureATTR =>
          val sig = pool.getExternalName(in.nextChar)
          newType = sigToType(sig, sym, isVarargs)
          if (ctx.debug && ctx.verbose)
            println("" + sym + "; signature = " + sig + " type = " + newType)
        case tpnme.SyntheticATTR =>
          sym.setFlag(Flags.SyntheticArtifact)
        case tpnme.BridgeATTR =>
          sym.setFlag(Flags.Bridge)
        case tpnme.DeprecatedATTR =>
          val msg = Literal(Constant("see corresponding Javadoc for more information."))
          val since = Literal(Constant(""))
          sym.addAnnotation(Annotation(defn.DeprecatedAnnot, msg, since))
        case tpnme.ConstantValueATTR =>
          val c = pool.getConstant(in.nextChar, symtype)
          if (c ne null) newType = ConstantType(c)
          else report.warning(s"Invalid constant in attribute of ${sym.showLocated} while parsing ${classfile}")
        case tpnme.AnnotationDefaultATTR =>
          sym.addAnnotation(Annotation(defn.AnnotationDefaultAnnot, NIL))
        // Java annotations on classes / methods / fields with RetentionPolicy.RUNTIME
        case tpnme.RuntimeVisibleAnnotationATTR
          | tpnme.RuntimeInvisibleAnnotationATTR =>
          parseAnnotations(attrLen)

        // TODO 1: parse runtime visible annotations on parameters
        // case tpnme.RuntimeParamAnnotationATTR

        // TODO 2: also parse RuntimeInvisibleParamAnnotation
        // i.e. java annotations with RetentionPolicy.CLASS?

        case tpnme.ExceptionsATTR =>
          parseExceptions(attrLen)

        case tpnme.CodeATTR =>
          if (sym.owner.isAllOf(Flags.JavaInterface)) {
            sym.resetFlag(Flags.Deferred)
            sym.owner.resetFlag(Flags.PureInterface)
            report.log(s"$sym in ${sym.owner} is a java8+ default method.")
          }
          in.skip(attrLen)

        case _ =>
      }
      in.bp = end
    }

    /**
     * Parse the "Exceptions" attribute which denotes the exceptions
     * thrown by a method.
     */
    def parseExceptions(len: Int): Unit = {
      val nClasses = in.nextChar
      for (n <- 0 until nClasses) {
        // FIXME: this performs an equivalent of getExceptionTypes instead of getGenericExceptionTypes (SI-7065)
        val cls = pool.getClassSymbol(in.nextChar.toInt)
        sym.addAnnotation(ThrowsAnnotation(cls.asClass))
      }
    }

    /** Parse a sequence of annotations and attaches them to the
     *  current symbol sym, except for the ScalaSignature annotation that it returns, if it is available. */
    def parseAnnotations(len: Int): Unit = {
      val nAttr = in.nextChar
      for (n <- 0 until nAttr)
        parseAnnotation(in.nextChar) match {
          case Some(annot) =>
            sym.addAnnotation(annot)
          case None =>
        }
    }

    // begin parseAttributes
    for (i <- 0 until in.nextChar)
      parseAttribute()

    cook.apply(newType)
  }

  /** Annotations in Scala are assumed to get all their arguments as constructor
   *  parameters. For Java annotations we need to fake it by making up the constructor.
   */
  def addAnnotationConstructor(classInfo: TempClassInfoType)(using Context): Unit =
    newSymbol(
      owner = classRoot.symbol,
      name = nme.CONSTRUCTOR,
      flags = Flags.Synthetic | Flags.JavaDefined | Flags.Method,
      info = new AnnotConstructorCompleter(classInfo)
    ).entered

  class AnnotConstructorCompleter(classInfo: TempClassInfoType) extends LazyType {
    def complete(denot: SymDenotation)(using Context): Unit = {
      val attrs = classInfo.decls.filter(sym => sym.isTerm && sym != denot.symbol).toScalaList
      val paramNames = attrs.map(_.name.asTermName)
      val paramTypes = attrs.map(_.info.resultType)
      denot.info = MethodType(paramNames, paramTypes, classRoot.typeRef)
    }
  }

  /** Enter own inner classes in the right scope. It needs the scopes to be set up,
   *  and implicitly current class' superclasses.
   */
  private def enterOwnInnerClasses()(using Context): Unit = {
    def className(name: Name): Name = {
      val name1 = name.toSimpleName
      name1.drop(name1.lastIndexOf('.') + 1)
    }

    def enterClassAndModule(entry: InnerClassEntry, file: AbstractFile, jflags: Int) =
      SymbolLoaders.enterClassAndModule(
          getOwner(jflags),
          entry.originalName,
          new ClassfileLoader(file),
          classTranslation.flags(jflags),
          getScope(jflags))

    for entry <- innerClasses.valuesIterator do
      // create a new class member for immediate inner classes
      if entry.outerName == currentClassName then
        val file = ctx.platform.classPath.findClassFile(entry.externalName.toString) getOrElse {
          throw new AssertionError(entry.externalName)
        }
        enterClassAndModule(entry, file, entry.jflags)
  }

  // Nothing$ and Null$ were incorrectly emitted with a Scala attribute
  // instead of ScalaSignature before 2.13.0-M2, see https://github.com/scala/scala/pull/5952
  private val scalaUnpickleWhitelist = List(tpnme.nothingClass, tpnme.nullClass)

  /** Parse inner classes. Expects `in.bp` to point to the superclass entry.
   *  Restores the old `bp`.
   *  @return true iff classfile is from Scala, so no Java info needs to be read.
   */
  def unpickleOrParseInnerClasses()(using Context): Option[Embedded] = {
    val oldbp = in.bp
    try {
      skipSuperclasses()
      skipMembers() // fields
      skipMembers() // methods
      val attrs = in.nextChar
      val attrbp = in.bp

      def scan(target: TypeName): Boolean = {
        in.bp = attrbp
        var i = 0
        while (i < attrs && pool.getName(in.nextChar).toTypeName != target) {
          val attrLen = in.nextInt
          in.skip(attrLen)
          i += 1
        }
        i < attrs
      }

      def unpickleScala(bytes: Array[Byte]): Some[Embedded] = {
        val allowed = ctx.settings.Yscala2Unpickler.value

        def failUnless(cond: Boolean) =
          assert(cond,
            s"Unpickling ${classRoot.symbol.showLocated} from ${classRoot.symbol.associatedFile} is not allowed with -Yscala2-unpickler $allowed")

        if (allowed != "always") {
          failUnless(allowed != "never")
          val allowedList = allowed.split(java.io.File.pathSeparator).toList
          val file = classRoot.symbol.associatedFile
          // Using `.toString.contains` isn't great, but it's good enough for a debug flag.
          failUnless(file == null || allowedList.exists(path => file.toString.contains(path)))
        }

        val unpickler = new unpickleScala2.Scala2Unpickler(bytes, classRoot, moduleRoot)(ctx)
        withMode(Scala2UnpicklingMode)(unpickler.run())
        Some(unpickler)
      }

      def unpickleTASTY(bytes: Array[Byte]): Some[Embedded]  = {
        val unpickler = new tasty.DottyUnpickler(bytes)
        unpickler.enter(roots = Set(classRoot, moduleRoot, moduleRoot.sourceModule))(using ctx.withSource(util.NoSource))
        Some(unpickler)
      }

      def parseScalaSigBytes: Array[Byte] = {
        val tag = in.nextByte.toChar
        assert(tag == STRING_TAG, tag)
        pool getBytes in.nextChar
      }

      def parseScalaLongSigBytes: Array[Byte] = {
        val tag = in.nextByte.toChar
        assert(tag == ARRAY_TAG, tag)
        val stringCount = in.nextChar
        val entries =
          for (i <- 0 until stringCount) yield {
            val stag = in.nextByte.toChar
            assert(stag == STRING_TAG, stag)
            in.nextChar.toInt
          }
        pool.getBytes(entries.toList)
      }

      if (scan(tpnme.TASTYATTR)) {
        val attrLen = in.nextInt
        val bytes = in.nextBytes(attrLen)
        if (attrLen == 16) { // A tasty attribute with that has only a UUID (16 bytes) implies the existence of the .tasty file
          val tastyBytes: Array[Byte] = classfile match { // TODO: simplify when #3552 is fixed
            case classfile: io.ZipArchive#Entry => // We are in a jar
              val path = classfile.parent.lookupName(
                classfile.name.stripSuffix(".class") + ".tasty", directory = false
              )
              if (path != null) {
                val stream = path.input
                try {
                  val tastyOutStream = new ByteArrayOutputStream()
                  val buffer = new Array[Byte](1024)
                  var read = stream.read(buffer, 0, buffer.length)
                  while (read != -1) {
                    tastyOutStream.write(buffer, 0, read)
                    read = stream.read(buffer, 0, buffer.length)
                  }
                  tastyOutStream.flush()
                  tastyOutStream.toByteArray
                } finally {
                  stream.close()
                }
              }
              else {
                report.error(s"Could not find $path in ${classfile.underlyingSource}")
                Array.empty
              }
            case _ =>
              if (classfile.jpath == null) {
                report.error("Could not load TASTY from .tasty for virtual file " + classfile)
                Array.empty
              } else {
                val plainFile = new PlainFile(io.File(classfile.jpath).changeExtension("tasty"))
                if (plainFile.exists) plainFile.toByteArray
                else {
                  report.error("Could not find " + plainFile)
                  Array.empty
                }
              }
          }
          if (tastyBytes.nonEmpty) {
            val reader = new TastyReader(bytes, 0, 16)
            val expectedUUID = new UUID(reader.readUncompressedLong(), reader.readUncompressedLong())
            val tastyUUID = new TastyHeaderUnpickler(tastyBytes).readHeader()
            if (expectedUUID != tastyUUID)
              report.warning(s"$classfile is out of sync with its TASTy file. Loaded TASTy file. Try cleaning the project to fix this issue", NoSourcePosition)
            return unpickleTASTY(tastyBytes)
          }
        }
        else return unpickleTASTY(bytes)
      }

      if (scan(tpnme.ScalaATTR) && !scalaUnpickleWhitelist.contains(classRoot.name))
        // To understand the situation, it's helpful to know that:
        // - Scalac emits the `ScalaSig` attribute for classfiles with pickled information
        // and the `Scala` attribute for everything else.
        // - Dotty emits the `TASTY` attribute for classfiles with pickled information
        // and the `Scala` attribute for _every_ classfile.
        //
        // Therefore, if the `Scala` attribute is present but the `TASTY`
        // attribute isn't, this classfile is a compilation artifact.
        return Some(NoEmbedded)

      if (scan(tpnme.RuntimeVisibleAnnotationATTR) || scan(tpnme.RuntimeInvisibleAnnotationATTR)) {
        val attrLen = in.nextInt
        val nAnnots = in.nextChar
        var i = 0
        while (i < nAnnots) {
          val attrClass = pool.getType(in.nextChar).typeSymbol
          val nArgs = in.nextChar
          var j = 0
          while (j < nArgs) {
            val argName = pool.getName(in.nextChar)
            if (argName == nme.bytes) {
              if (attrClass == defn.ScalaSignatureAnnot)
                return unpickleScala(parseScalaSigBytes)
              else if (attrClass == defn.ScalaLongSignatureAnnot)
                return unpickleScala(parseScalaLongSigBytes)
              else if (attrClass == defn.TASTYSignatureAnnot)
                return unpickleTASTY(parseScalaSigBytes)
              else if (attrClass == defn.TASTYLongSignatureAnnot)
                return unpickleTASTY(parseScalaLongSigBytes)
            }
            parseAnnotArg(skip = true)
            j += 1
          }
          i += 1
        }
      }

      if (scan(tpnme.InnerClassesATTR)) {
        val attrLen = in.nextInt
        val entries = in.nextChar.toInt
        for (i <- 0 until entries) {
          val innerIndex = in.nextChar
          val outerIndex = in.nextChar
          val nameIndex = in.nextChar
          val jflags = in.nextChar
          if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0) {
            val entry = InnerClassEntry(innerIndex, outerIndex, nameIndex, jflags)
            innerClasses(pool.getClassName(innerIndex)) = entry
          }
        }
      }
      None
    }
    finally in.bp = oldbp
  }

  /** An entry in the InnerClasses attribute of this class file. */
  case class InnerClassEntry(external: Int, outer: Int, name: Int, jflags: Int) {
    def externalName: SimpleName = pool.getClassName(external)
    def outerName: SimpleName    = pool.getClassName(outer)
    def originalName: SimpleName = pool.getName(name)

    override def toString: String =
      s"$originalName in $outerName($externalName)"
  }

  object innerClasses extends util.HashMap[Name, InnerClassEntry] {
    /** Return the Symbol of the top level class enclosing `name`,
     *  or 'name's symbol if no entry found for `name`.
     */
    def topLevelClass(name: Name)(using Context): Symbol = {
      val tlName = if (contains(name)) {
        var entry = this(name)
        while (contains(entry.outerName))
          entry = this(entry.outerName)
        entry.outerName
      }
      else
        name
      classNameToSymbol(tlName)
    }

    /** Return the class symbol for `entry`. It looks it up in its outer class.
     *  This might force outer class symbols.
     */
    def classSymbol(entry: InnerClassEntry)(using Context): Symbol = {
      def getMember(sym: Symbol, name: Name)(using Context): Symbol =
        if (isStatic(entry.jflags))
          if (sym == classRoot.symbol)
            staticScope.lookup(name)
          else {
            var module = sym.companionModule
            if (!module.exists && sym.isAbsent())
              module = sym.scalacLinkedClass
            module.info.member(name).symbol
          }
        else if (sym == classRoot.symbol)
          instanceScope.lookup(name)
        else
          sym.info.member(name).symbol

      val outerName = entry.outerName.stripModuleClassSuffix
      val innerName = entry.originalName
      val owner = classNameToSymbol(outerName)
      val result = atPhase(typerPhase)(getMember(owner, innerName.toTypeName))
      assert(result ne NoSymbol,
        i"""failure to resolve inner class:
           |externalName = ${entry.externalName},
           |outerName = $outerName,
           |innerName = $innerName
           |owner.fullName = ${owner.showFullName}
           |while parsing ${classfile}""")
      result
    }
  }

  def skipAttributes(): Unit = {
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  def skipMembers(): Unit = {
    val memberCount = in.nextChar
    for (i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }

  def skipSuperclasses(): Unit = {
    in.skip(2) // superclass
    val ifaces = in.nextChar
    in.skip(2 * ifaces)
  }

  protected def getOwner(flags: Int): Symbol =
    if (isStatic(flags)) moduleRoot.symbol else classRoot.symbol

  protected def getScope(flags: Int): MutableScope =
    if (isStatic(flags)) staticScope else instanceScope

  private def getPrivateWithin(jflags: Int)(using Context): Symbol =
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PUBLIC)) == 0)
      classRoot.enclosingPackageClass
    else
      NoSymbol

  private def isPrivate(flags: Int)     = (flags & JAVA_ACC_PRIVATE) != 0
  private def isStatic(flags: Int)      = (flags & JAVA_ACC_STATIC) != 0
  private def hasAnnotation(flags: Int) = (flags & JAVA_ACC_ANNOTATION) != 0

  class ConstantPool {
    private val len = in.nextChar
    private val starts = new Array[Int](len)
    private val values = new Array[AnyRef](len)
    private val internalized = new Array[SimpleName](len)

    { var i = 1
      while (i < starts.length) {
        starts(i) = in.bp
        i += 1
        (in.nextByte.toInt: @switch) match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar)
          case CONSTANT_CLASS | CONSTANT_STRING | CONSTANT_METHODTYPE =>
            in.skip(2)
          case CONSTANT_METHODHANDLE =>
            in.skip(3)
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF
             | CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT
             | CONSTANT_INVOKEDYNAMIC =>
            in.skip(4)
          case CONSTANT_LONG | CONSTANT_DOUBLE =>
            in.skip(8)
            i += 1
          case _ =>
            errorBadTag(in.bp - 1)
        }
      }
    }

    /** Return the name found at given index. */
    def getName(index: Int): SimpleName = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      values(index) match {
        case name: SimpleName => name
        case null   =>
          val start = starts(index)
          if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
          val len   = in.getChar(start + 1).toInt
          val name = termName(fromMUTF8(in.buf, start + 1, len + 2))
          values(index) = name
          name
      }
    }

    private def fromMUTF8(bytes: Array[Byte], offset: Int, len: Int): String =
      new DataInputStream(new ByteArrayInputStream(bytes, offset, len)).readUTF

    /** Return the name found at given index in the constant pool, with '/' replaced by '.'. */
    def getExternalName(index: Int): SimpleName = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      if (internalized(index) == null)
        internalized(index) = getName(index).replace('/', '.')

      internalized(index)
    }

    def getClassSymbol(index: Int)(using Context): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var c = values(index).asInstanceOf[Symbol]
      if (c eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name.endsWith("$") && (name ne nme.nothingRuntimeClass) && (name ne nme.nullRuntimeClass))
          // Null$ and Nothing$ ARE classes
          c = requiredModule(name.dropRight(1))
        else c = classNameToSymbol(name)
        values(index) = c
      }
      c
    }

    /** Return the external name of the class info structure found at 'index'.
     *  Use 'getClassSymbol' if the class is sure to be a top-level class.
     */
    def getClassName(index: Int): SimpleName = {
      val start = starts(index)
      if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
      getExternalName(in.getChar(start + 1))
    }

    /** Return the type of a class constant entry. Since
     *  arrays are considered to be class types, they might
     *  appear as entries in 'newarray' or 'cast' opcodes.
     */
    def getClassOrArrayType(index: Int)(using Context): Type = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      val value = values(index)
      var c: Type = null
      if (value eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name.firstPart(0) == ARRAY_TAG) {
          c = sigToType(name)
          values(index) = c
        }
        else {
          val sym = classNameToSymbol(name)
          values(index) = sym
          c = sym.typeRef
        }
      }
      else c = value match {
          case tp: Type => tp
          case cls: Symbol => cls.typeRef
      }
      c
    }

    def getType(index: Int, isVarargs: Boolean = false)(using Context): Type =
      sigToType(getExternalName(index), isVarargs = isVarargs)

    def getSuperClass(index: Int)(using Context): Symbol = {
      assert(index != 0, "attempt to parse java.lang.Object from classfile")
      getClassSymbol(index)
    }

    def getConstant(index: Int, pt: Type = WildcardType)(using Context): Constant = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index)
      if (value eq null) {
        val start = starts(index)
        value = (in.buf(start).toInt: @switch) match {
          case CONSTANT_STRING =>
            Constant(getName(in.getChar(start + 1).toInt).toString)
          case CONSTANT_INTEGER =>
            Constant(in.getInt(start + 1))
          case CONSTANT_FLOAT =>
            Constant(in.getFloat(start + 1))
          case CONSTANT_LONG =>
            Constant(in.getLong(start + 1))
          case CONSTANT_DOUBLE =>
            Constant(in.getDouble(start + 1))
          case CONSTANT_CLASS =>
            getClassOrArrayType(index).typeSymbol
          case _ =>
            errorBadTag(start)
        }
        values(index) = value
      }
      value match {
        case ct: Constant =>
          if pt ne WildcardType then
            // As specified in https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.16.1,
            // an annotation argument of type boolean, byte, char or short will
            // be represented as a CONSTANT_INTEGER, so we need to convert it to
            // produce a correctly-typed tree. We need to do this each time the
            // constant is accessed instead of storing the result of the
            // conversion in the `values` cache, because the same constant might
            // be used for annotation arguments of different type.
            if (pt eq defn.BooleanType) && ct.tag == IntTag then
              Constant(ct.value != 0)
            else
              ct.convertTo(pt)
          else
            ct
        case cls: Symbol   => Constant(cls.typeRef)
        case arr: Type     => Constant(arr)
      }
    }

    private def getSubArray(bytes: Array[Byte]): Array[Byte] = {
      val decodedLength = ByteCodecs.decode(bytes)
      val arr           = new Array[Byte](decodedLength)
      System.arraycopy(bytes, 0, arr, 0, decodedLength)
      arr
    }

    def getBytes(index: Int): Array[Byte] = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index).asInstanceOf[Array[Byte]]
      if (value eq null) {
        val start = starts(index)
        if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
        val len   = in.getChar(start + 1)
        val bytes = new Array[Byte](len)
        System.arraycopy(in.buf, start + 3, bytes, 0, len)
        value = getSubArray(bytes)
        values(index) = value
      }
      value
    }

    def getBytes(indices: List[Int]): Array[Byte] = {
      assert(!indices.isEmpty, indices)
      var value = values(indices.head).asInstanceOf[Array[Byte]]
      if (value eq null) {
        val bytesBuffer = ArrayBuffer.empty[Byte]
        for (index <- indices) {
          if (index <= 0 || ConstantPool.this.len <= index) errorBadIndex(index)
          val start = starts(index)
          if (in.buf(start).toInt != CONSTANT_UTF8) errorBadTag(start)
          val len = in.getChar(start + 1)
          bytesBuffer ++= in.buf.view.slice(start + 3, start + 3 + len)
        }
        value = getSubArray(bytesBuffer.toArray)
        values(indices.head) = value
      }
      value
    }

    /** Throws an exception signaling a bad constant index. */
    private def errorBadIndex(index: Int) =
      throw new RuntimeException("bad constant pool index: " + index + " at pos: " + in.bp)

    /** Throws an exception signaling a bad tag at given address. */
    private def errorBadTag(start: Int) =
      throw new RuntimeException("bad constant pool tag " + in.buf(start) + " at byte " + start)
  }
}
