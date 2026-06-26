package dotty.tools.dotc.core

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.TypeOps
import dotty.tools.dotc.core.Types.*

import org.junit.Test
import org.junit.Assert.{assertEquals, assertSame}

class TypesTest extends DottyTest:

  @Test def tuple3TypeSize =
    val tpe = defn.TupleType(3).nn.appliedTo(Vector(defn.IntType, defn.BooleanType, defn.DoubleType))
    assertEquals(3, tpe.typeSize)

  @Test def tuple3ConsTypeSize =
    val tpe = TypeOps.nestedPairs(Vector(defn.IntType, defn.BooleanType, defn.DoubleType))
    assertEquals(3, tpe.typeSize)

  @Test def nestedPairsElements =
    assertEquals(Vector(), TypeOps.nestedPairs(Vector()).tupleElementTypes.get)
    assertEquals(Vector(defn.IntType), TypeOps.nestedPairs(Vector(defn.IntType)).tupleElementTypes.get)
    assertEquals(
      Vector(defn.IntType, defn.BooleanType, defn.DoubleType),
      TypeOps.nestedPairs(Vector(defn.IntType, defn.BooleanType, defn.DoubleType)).tupleElementTypes.get
    )

  @Test def substSymLargeDuplicateUsesFirstMatch =
    val from = Vector(defn.IntClass) ++ Vector.fill(31)(defn.AnyClass) ++ Vector(defn.IntClass)
    val to = Vector(defn.StringClass) ++ Vector.fill(31)(defn.AnyClass) ++ Vector(defn.BooleanClass)
    val mapped = defn.IntType.substSym(from, to)
    assertSame(defn.StringClass, mapped.typeSymbol)
