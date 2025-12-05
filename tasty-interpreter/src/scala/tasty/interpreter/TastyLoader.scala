package scala.tasty.interpreter

import scala.quoted.*
import scala.collection.mutable

/**
 * Loads definitions from TASTy files on the classpath.
 * This is the foundation for pure TASTy interpretation without JVM reflection.
 */
class TastyLoader[Q <: Quotes & Singleton](using val q: Q) {
  import q.reflect.*

  // Cache loaded class/module definitions by full name
  private val classCache = mutable.Map[String, ClassDef]()
  private val moduleCache = mutable.Map[String, (Symbol, ValDef)]()  // Module symbol -> its definition
  private val defCache = mutable.Map[Symbol, DefDef]()

  /**
   * Load a class definition by its full name.
   * Returns None if the class doesn't have TASTy available.
   */
  def loadClass(fullName: String): Option[ClassDef] = {
    classCache.get(fullName).orElse {
      // Try to load from symbol
      val sym = Symbol.classSymbol(fullName)
      if (sym.exists) {
        sym.tree match {
          case classDef: ClassDef =>
            classCache(fullName) = classDef
            Some(classDef)
          case _ => None
        }
      } else None
    }
  }

  /**
   * Load a module (object) definition by its full name.
   */
  def loadModule(fullName: String): Option[(Symbol, ValDef)] = {
    moduleCache.get(fullName).orElse {
      val sym = Symbol.classSymbol(fullName + "$")  // Module classes have $ suffix
      if (sym.exists && sym.flags.is(Flags.Module)) {
        sym.tree match {
          case classDef: ClassDef =>
            // Find the module val definition in the companion
            val moduleSym = sym.companionModule
            if (moduleSym.exists) {
              moduleSym.tree match {
                case valDef: ValDef =>
                  moduleCache(fullName) = (moduleSym, valDef)
                  Some((moduleSym, valDef))
                case _ => None
              }
            } else None
          case _ => None
        }
      } else None
    }
  }

  /**
   * Load a method definition from a class/module.
   */
  def loadMethod(classSym: Symbol, methodName: String): Option[DefDef] = {
    val methods = classSym.memberMethods.filter(_.name == methodName)
    methods.headOption.flatMap { methodSym =>
      defCache.get(methodSym).orElse {
        methodSym.tree match {
          case ddef: DefDef if ddef.rhs.isDefined =>
            defCache(methodSym) = ddef
            Some(ddef)
          case _ => None
        }
      }
    }
  }

  /**
   * Load a method definition by symbol.
   */
  def loadMethodDef(sym: Symbol): Option[DefDef] = {
    defCache.get(sym).orElse {
      sym.tree match {
        case ddef: DefDef if ddef.rhs.isDefined =>
          defCache(sym) = ddef
          Some(ddef)
        case _ => None
      }
    }
  }

  /**
   * Load a val definition by symbol.
   */
  def loadValDef(sym: Symbol): Option[ValDef] = {
    sym.tree match {
      case vdef: ValDef => Some(vdef)
      case _ => None
    }
  }

  /**
   * Check if TASTy is available for a symbol.
   * TASTy is available if the symbol's tree is not EmptyTree.
   */
  def hasTasty(sym: Symbol): Boolean = {
    sym.tree match {
      case tree if tree.isInstanceOf[ClassDef] || tree.isInstanceOf[DefDef] || tree.isInstanceOf[ValDef] => true
      case _ => false
    }
  }

  /**
   * Get all member methods of a class that have TASTy bodies.
   */
  def getMemberMethods(classSym: Symbol): List[DefDef] = {
    classSym.memberMethods.flatMap { methodSym =>
      methodSym.tree match {
        case ddef: DefDef if ddef.rhs.isDefined => Some(ddef)
        case _ => None
      }
    }
  }
}

