package tasty

import TastyBuffer._
import TastyFormat._
import interpreter.Ast
import interpreter.Ast._

/**
 * Unpickler that converts TASTy ASTs to interpreter ASTs.
 */
class TastyAstUnpickler(unpickler: TastyUnpickler) {

  private var reader: TastyReader = _
  private var sharedTerms: scala.collection.mutable.Map[Int, Ast] = _
  private var sharedTypes: scala.collection.mutable.Map[Int, String] = _

  /**
   * Unpickle the main method body from the TASTy file.
   */
  def unpickleMain(): Option[Ast] = {
    unpickler.getASTsSection match {
      case Some(section) =>
        reader = section.reader
        sharedTerms = scala.collection.mutable.Map.empty
        sharedTypes = scala.collection.mutable.Map.empty

        try {
          // Find and unpickle the main method
          findMain()
        } catch {
          case e: Exception =>
            println(s"Error unpickling TASTy: ${e.getMessage}")
            e.printStackTrace()
            None
        }

      case None =>
        println("No ASTs section found")
        None
    }
  }

  /**
   * Find the main method in the TASTy tree.
   */
  private def findMain(): Option[Ast] = {
    while (!reader.isAtEnd) {
      val tag = reader.readByte()

      if (tag == PACKAGE) {
        val end = reader.readEnd()
        // Skip package path
        skipTree()
        // Look for main in package contents
        val result = findMainInPackage(end)
        if (result.isDefined) return result
        reader.goto(end)
      } else {
        skipTreeWithTag(tag)
      }
    }
    None
  }

  /**
   * Find main method within a package.
   */
  private def findMainInPackage(packageEnd: Addr): Option[Ast] = {
    while (reader.currentAddr.index < packageEnd.index) {
      val tag = reader.readByte()

      if (tag == TYPEDEF) {
        val end = reader.readEnd()
        val nameRef = reader.readNat()
        val name = unpickler.nameToString(nameRef)

        // Check if this is an object (module)
        val nextTag = reader.nextByte
        if (nextTag == TEMPLATE) {
          val result = findMainInTemplate(name)
          if (result.isDefined) return result
        }
        reader.goto(end)
      } else {
        skipTreeWithTag(tag)
      }
    }
    None
  }

  /**
   * Find main method within a template (class body).
   */
  private def findMainInTemplate(className: String): Option[Ast] = {
    val tag = reader.readByte()
    if (tag != TEMPLATE) {
      return None
    }

    val end = reader.readEnd()

    // Skip type params
    while (reader.nextByte == TYPEPARAM) {
      skipTree()
    }

    // Skip term params
    while (reader.nextByte == PARAM || reader.nextByte == EMPTYCLAUSE || reader.nextByte == SPLITCLAUSE) {
      skipTree()
    }

    // Skip parents
    while (!isStatTag(reader.nextByte) && reader.nextByte != SELFDEF &&
           reader.currentAddr.index < end.index) {
      skipTree()
    }

    // Skip SELFDEF if present
    if (reader.nextByte == SELFDEF) {
      skipTree()
    }

    // Skip SPLITCLAUSE if present (marks end of parents)
    if (reader.nextByte == SPLITCLAUSE) {
      reader.readByte()
    }

    // Look through statements for main method
    while (reader.currentAddr.index < end.index) {
      val statTag = reader.readByte()

      if (statTag == DEFDEF) {
        val defEnd = reader.readEnd()
        val nameRef = reader.readNat()
        val methodName = unpickler.nameToString(nameRef)

        if (methodName == "main") {
          // Found main! Now unpickle its body
          // Skip params
          while (reader.nextByte == PARAM || reader.nextByte == TYPEPARAM ||
                 reader.nextByte == EMPTYCLAUSE || reader.nextByte == SPLITCLAUSE) {
            skipTree()
          }

          // Skip return type
          skipTree()

          // Read body if present
          if (reader.currentAddr.index < defEnd.index && !isModifierTag(reader.nextByte)) {
            val body = readTree()
            return Some(body)
          }
        }
        reader.goto(defEnd)
      } else {
        skipTreeWithTag(statTag)
      }
    }

    None
  }

  /**
   * Read a tree and convert to interpreter AST.
   */
  private def readTree(): Ast = {
    val addr = reader.currentAddr
    val tag = reader.readByte()
    readTreeWithTag(tag, addr)
  }

  /**
   * Read a tree with known tag.
   */
  private def readTreeWithTag(tag: Int, addr: Addr): Ast = {
    tag match {
      // Constants
      case UNITconst => UnitLit
      case FALSEconst => BoolLit(false)
      case TRUEconst => BoolLit(true)
      case NULLconst => NullLit
      case BYTEconst => IntLit(reader.readInt())
      case SHORTconst => IntLit(reader.readInt())
      case CHARconst => CharLit(reader.readNat().toChar)
      case INTconst => IntLit(reader.readInt())
      case LONGconst => LongLit(reader.readLongInt())
      case FLOATconst =>
        val bits = reader.readInt()
        FloatLit(java.lang.Float.intBitsToFloat(bits))
      case DOUBLEconst =>
        val bits = reader.readLongInt()
        DoubleLit(java.lang.Double.longBitsToDouble(bits))
      case STRINGconst =>
        val nameRef = reader.readNat()
        StringLit(unpickler.nameToString(nameRef))

      // Shared references
      case SHAREDterm =>
        val ref = reader.readNat()
        sharedTerms.getOrElse(ref, UnitLit)
      case SHAREDtype =>
        reader.readNat() // Skip type ref
        UnitLit

      // References
      case IDENT =>
        val nameRef = reader.readNat()
        skipTree() // Skip type
        Ident(unpickler.nameToString(nameRef))

      case SELECT =>
        val nameRef = reader.readNat()
        val qual = readTree()
        Select(qual, unpickler.nameToString(nameRef))

      case TERMREFdirect | TYPEREFdirect =>
        reader.readNat() // Skip symbol ref
        Ident("_direct_")

      case TERMREFpkg | TYPEREFpkg =>
        val nameRef = reader.readNat()
        Ident(unpickler.nameToString(nameRef))

      case TERMREF | TYPEREF =>
        val nameRef = reader.readNat()
        skipTree() // Skip qualifier type
        Ident(unpickler.nameToString(nameRef))

      case THIS =>
        skipTree() // Skip class type
        Ident("this")

      case NEW =>
        val tpe = readTree()
        val className = tpe match {
          case Ident(n) => n
          case Select(_, n) => n
          case _ => "Unknown"
        }
        New(className, Nil)

      // Control flow
      case BLOCK =>
        val end = reader.readEnd()
        val expr = readTree()
        val stats = readTreesUntil(end)
        Block(stats, expr)

      case IF =>
        val end = reader.readEnd()
        // Check for INLINE tag
        if (reader.nextByte == INLINE) reader.readByte()
        val cond = readTree()
        val thenp = readTree()
        val elsep = if (reader.currentAddr.index < end.index) readTree() else UnitLit
        If(cond, thenp, elsep)

      case WHILE =>
        val end = reader.readEnd()
        val cond = readTree()
        val body = readTree()
        While(cond, body)

      case MATCH =>
        val end = reader.readEnd()
        // Handle IMPLICIT, INLINE, SUBMATCH
        while (reader.nextByte == IMPLICIT || reader.nextByte == INLINE || reader.nextByte == SUBMATCH) {
          reader.readByte()
        }
        val selector = readTree()
        val cases = readCaseDefsUntil(end)
        Match(selector, cases)

      case TRY =>
        val end = reader.readEnd()
        val block = readTree()
        val catches = scala.collection.mutable.ListBuffer[CaseDef]()
        while (reader.nextByte == CASEDEF && reader.currentAddr.index < end.index) {
          catches += readCaseDef()
        }
        val finalizer = if (reader.currentAddr.index < end.index) Some(readTree()) else None
        Try(block, catches.toList, finalizer)

      case RETURN =>
        val end = reader.readEnd()
        reader.readNat() // Skip method ref
        val expr = if (reader.currentAddr.index < end.index) readTree() else UnitLit
        Return(expr)

      case THROW =>
        val expr = readTree()
        Throw(expr)

      // Applications
      case APPLY =>
        val end = reader.readEnd()
        val fn = readTree()
        val args = readTreesUntil(end)
        Apply(fn, args)

      case TYPEAPPLY =>
        val end = reader.readEnd()
        val fn = readTree()
        // Skip type arguments
        reader.goto(end)
        fn

      case TYPED =>
        val end = reader.readEnd()
        val expr = readTree()
        // Skip ascription type
        reader.goto(end)
        expr

      case ASSIGN =>
        val end = reader.readEnd()
        val lhs = readTree()
        val rhs = readTree()
        val name = lhs match {
          case Ident(n) => n
          case Select(_, n) => n
          case _ => "_"
        }
        Assign(name, rhs)

      case NAMEDARG =>
        val nameRef = reader.readNat()
        readTree() // Read the argument value

      // Definitions
      case VALDEF =>
        val end = reader.readEnd()
        val nameRef = reader.readNat()
        val name = unpickler.nameToString(nameRef)
        skipTree() // Skip type
        val rhs = if (reader.currentAddr.index < end.index && !isModifierTag(reader.nextByte)) {
          readTree()
        } else {
          UnitLit
        }
        // Check for MUTABLE modifier
        val mutable = readModifiersUntil(end).contains(MUTABLE)
        ValDef(name, rhs, mutable)

      case DEFDEF =>
        val end = reader.readEnd()
        val nameRef = reader.readNat()
        val name = unpickler.nameToString(nameRef)

        // Read params
        val params = scala.collection.mutable.ListBuffer[String]()
        while (reader.nextByte == PARAM || reader.nextByte == TYPEPARAM ||
               reader.nextByte == EMPTYCLAUSE || reader.nextByte == SPLITCLAUSE) {
          val paramTag = reader.readByte()
          if (paramTag == PARAM) {
            val paramEnd = reader.readEnd()
            val paramNameRef = reader.readNat()
            params += unpickler.nameToString(paramNameRef)
            reader.goto(paramEnd)
          } else if (paramTag == TYPEPARAM) {
            val paramEnd = reader.readEnd()
            reader.goto(paramEnd)
          }
          // Skip EMPTYCLAUSE and SPLITCLAUSE
        }

        // Skip return type
        skipTree()

        // Read body
        val body = if (reader.currentAddr.index < end.index && !isModifierTag(reader.nextByte)) {
          readTree()
        } else {
          UnitLit
        }

        reader.goto(end)
        DefDef(name, params.toList, body)

      case LAMBDA =>
        val end = reader.readEnd()
        val meth = readTree()
        // Skip target type if present
        reader.goto(end)
        // The lambda body is in the nested DefDef
        meth match {
          case DefDef(_, params, body) => Lambda(params, body)
          case Block(List(d: DefDef), _) => Lambda(d.params, d.body)
          case _ => Lambda(Nil, meth)
        }

      case INLINED =>
        val end = reader.readEnd()
        val expr = readTree()
        // Skip call and bindings
        reader.goto(end)
        expr

      case REPEATED =>
        val end = reader.readEnd()
        skipTree() // Skip element type
        val elems = readTreesUntil(end)
        Apply(Ident("List"), elems)

      // Patterns
      case BIND =>
        val end = reader.readEnd()
        val nameRef = reader.readNat()
        skipTree() // Skip type
        val pattern = if (reader.currentAddr.index < end.index) readTree() else Ident("_")
        reader.goto(end)
        Ident(unpickler.nameToString(nameRef))

      case ALTERNATIVE =>
        val end = reader.readEnd()
        val alts = readTreesUntil(end)
        alts.headOption.getOrElse(UnitLit)

      case UNAPPLY =>
        val end = reader.readEnd()
        val fn = readTree()
        // Skip implicit args and type
        while (reader.nextByte == IMPLICITarg) {
          reader.readByte()
          skipTree()
        }
        skipTree() // pattern type
        val patterns = readTreesUntil(end)
        Apply(fn, patterns)

      // Type trees (mostly skip)
      case IDENTtpt | SELECTtpt | SINGLETONtpt | REFINEDtpt | APPLIEDtpt |
           LAMBDAtpt | TYPEBOUNDStpt | ANNOTATEDtpt | BYNAMEtpt | MATCHtpt |
           EXPLICITtpt =>
        skipTreeWithTag(tag)
        UnitLit

      case CASEDEF =>
        val end = reader.readEnd()
        val pattern = readTree()
        val body = readTree()
        val guard = if (reader.currentAddr.index < end.index) Some(readTree()) else None
        reader.goto(end)
        body // For now, just return the body

      // Skip unknown/unhandled tags
      case _ =>
        skipTreeWithTag(tag)
        UnitLit
    }
  }

  /**
   * Read trees until end address.
   */
  private def readTreesUntil(end: Addr): List[Ast] = {
    val buf = scala.collection.mutable.ListBuffer[Ast]()
    while (reader.currentAddr.index < end.index) {
      buf += readTree()
    }
    buf.toList
  }

  /**
   * Read case definitions until end address.
   */
  private def readCaseDefsUntil(end: Addr): List[CaseDef] = {
    val buf = scala.collection.mutable.ListBuffer[CaseDef]()
    while (reader.nextByte == CASEDEF && reader.currentAddr.index < end.index) {
      buf += readCaseDef()
    }
    buf.toList
  }

  /**
   * Read a single case definition.
   */
  private def readCaseDef(): CaseDef = {
    reader.readByte() // CASEDEF tag
    val end = reader.readEnd()
    val pattern = readPattern()
    val body = readTree()
    val guard = if (reader.currentAddr.index < end.index) Some(readTree()) else None
    reader.goto(end)
    CaseDef(pattern, guard, body)
  }

  /**
   * Read a pattern.
   */
  private def readPattern(): Pattern = {
    val tag = reader.readByte()

    tag match {
      case BIND =>
        val end = reader.readEnd()
        val nameRef = reader.readNat()
        skipTree() // type
        val inner = if (reader.currentAddr.index < end.index && !isModifierTag(reader.nextByte)) {
          Some(readPattern())
        } else None
        reader.goto(end)
        Pattern.Bind(unpickler.nameToString(nameRef), inner)

      case ALTERNATIVE =>
        val end = reader.readEnd()
        val patterns = scala.collection.mutable.ListBuffer[Pattern]()
        while (reader.currentAddr.index < end.index) {
          patterns += readPattern()
        }
        Pattern.Alternative(patterns.toList)

      case UNAPPLY =>
        val end = reader.readEnd()
        val fn = readTree()
        // Skip implicit args
        while (reader.nextByte == IMPLICITarg) {
          reader.readByte()
          skipTree()
        }
        skipTree() // pattern type
        val patterns = scala.collection.mutable.ListBuffer[Pattern]()
        while (reader.currentAddr.index < end.index) {
          patterns += readPattern()
        }
        val className = fn match {
          case Ident(n) => n
          case Select(_, n) => n
          case _ => "?"
        }
        Pattern.Unapply(className, patterns.toList)

      case TYPED =>
        val end = reader.readEnd()
        val inner = readPattern()
        val tpe = readTree()
        reader.goto(end)
        val typeName = tpe match {
          case Ident(n) => n
          case _ => "Any"
        }
        Pattern.Typed(typeName, Some(inner))

      case UNITconst => Pattern.Literal(())
      case FALSEconst => Pattern.Literal(false)
      case TRUEconst => Pattern.Literal(true)
      case NULLconst => Pattern.Literal(null)
      case BYTEconst => Pattern.Literal(reader.readInt().toByte)
      case SHORTconst => Pattern.Literal(reader.readInt().toShort)
      case CHARconst => Pattern.Literal(reader.readNat().toChar)
      case INTconst => Pattern.Literal(reader.readInt())
      case LONGconst => Pattern.Literal(reader.readLongInt())
      case STRINGconst =>
        val nameRef = reader.readNat()
        Pattern.Literal(unpickler.nameToString(nameRef))

      case IDENT =>
        val nameRef = reader.readNat()
        skipTree() // type
        val name = unpickler.nameToString(nameRef)
        if (name == "_") Pattern.Wildcard
        else Pattern.Bind(name, None)

      case _ =>
        // For other cases, skip and return wildcard
        skipTreeWithTag(tag)
        Pattern.Wildcard
    }
  }

  /**
   * Read modifiers until end.
   */
  private def readModifiersUntil(end: Addr): Set[Int] = {
    val mods = scala.collection.mutable.Set[Int]()
    while (reader.currentAddr.index < end.index && isModifierTag(reader.nextByte)) {
      val mod = reader.readByte()
      mods += mod
      // Handle qualified modifiers
      if (mod == PRIVATEqualified || mod == PROTECTEDqualified) {
        skipTree()
      } else if (mod == ANNOTATION) {
        val annEnd = reader.readEnd()
        reader.goto(annEnd)
      }
    }
    mods.toSet
  }

  /**
   * Skip a tree.
   */
  private def skipTree(): Unit = {
    val tag = reader.readByte()
    skipTreeWithTag(tag)
  }

  /**
   * Skip a tree with known tag.
   */
  private def skipTreeWithTag(tag: Int): Unit = {
    if (tag >= firstLengthTreeTag) {
      val end = reader.readEnd()
      reader.goto(end)
    } else if (tag >= firstNatASTTreeTag) {
      reader.readNat()
      skipTree()
    } else if (tag >= firstASTTreeTag) {
      skipTree()
    } else if (tag >= firstNatTreeTag) {
      reader.readNat()
    }
    // Simple tags (category 1) need no additional skipping
  }

  /**
   * Check if tag is a statement tag.
   */
  private def isStatTag(tag: Int): Boolean = {
    tag == VALDEF || tag == DEFDEF || tag == TYPEDEF || tag == IMPORT || tag == EXPORT ||
    (tag >= firstSimpleTreeTag && tag < firstNatTreeTag && !isModifierTag(tag))
  }
}

