package dotty.tools
package dotc
package core

import Contexts.*, Decorators.*, NameKinds.*, Symbols.*, Types.*
import ast.*, Trees.*
import printing.*, Texts.*

import scala.annotation.internal.sharable
import util.{SimpleIdentitySet, SimpleIdentityMap}

object AssumeInfoMap:
  @sharable val empty: AssumeInfoMap = AssumeInfoMap(SimpleIdentityMap.empty)

class AssumeInfoMap private (
  private val map: SimpleIdentityMap[Symbol, Type],
) extends Showable:
  def info(sym: Symbol)(using Context): Type | Null = map(sym)

  def add(sym: Symbol, info: Type) = new AssumeInfoMap(map.updated(sym, info))

  override def toText(p: Printer): Text =
    given Context = p match
      case p: PlainPrinter => p.printerContext
      case _               => Contexts.NoContext
    val deps = for (sym, info) <- map.toList yield
      (p.toText(sym.typeRef) ~ p.toText(info)).close
    ("AssumeInfo(" ~ Text(deps, ", ") ~ ")").close
