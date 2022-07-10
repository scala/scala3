package dotty.tools.dotc.util

import collection.mutable.Map as MMap

import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Contexts.Context

case class ShortTypeName(ref: String, applied: String, canonical: String)

final class TypeRegistry:
  def getShortName(tpe: Type)(using Context) = computeShortName(tpe)

  def getFullName(tpe: Type)(using Context) = 
    fullNames.getOrElseUpdate(tpe, {
      val concreteType = concrete(tpe)

      fullNames.getOrElseUpdate(concreteType, {
        computeFullName(concreteType)
      })
    })


  def allShortNames: Map[Type, ShortTypeName] = shortNames.toMap

  def allTypes: scala.collection.Set[Type] = shortNames.keySet

  private val fullNames = MMap.empty[Type, String]
  private val shortNames = MMap.empty[Type, ShortTypeName]
  private val resolvedNames = MMap.empty[ShortTypeName, String]
  private val conflicts = MMap.empty[ShortTypeName, Int]

  private def concrete(tpe: Type) = 
    tpe match 
    case at: AppliedType => at.tycon 
    case _ => tpe

  private def nameIt(canonical: String, template: String, tpe: Type): String =
    val cnt = shortNames.count((t, r) => r.canonical == canonical && tpe != t)
    val idx = cnt.toChar match 
      case 0 => ""
      case 1 => "¹"
      case 2 => "²"
      case 3 => "³"
      case 4 => "⁴"
      case 5 => "⁵"
      case 6 => "⁶"
      case 7 => "⁷"
      case 8 => "⁸"
      case 9 => "⁹"
    
    template.replaceAll("<cnt>", idx).nn

  private def computeShortName(tpe: Type)(using Context): ShortTypeName = 
    shortNames.getOrElseUpdate(concrete(tpe), {
      val short = 
        tpe match 
        case n: NamedType => 
          val nm = n.name.lastPart.show
          val nmTemplate = nm + "<cnt>"
          val value = nameIt(nm, nmTemplate, n)
          ShortTypeName(value, value, nm)

        case at: AppliedType => 
          val ref = computeShortName(at.tycon).ref
          val applied = ref + at.args.map(computeShortName(_).applied).mkString("[", ", ", "]")

          ShortTypeName(ref, applied, ref)

        case _ => ShortTypeName(tpe.show, tpe.show, tpe.show)

      short

    })

  private def computeFullName(tpe: Type)(using Context) = 
    tpe.show


