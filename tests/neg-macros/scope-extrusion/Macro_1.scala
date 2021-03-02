import quoted.*

inline def test1(): Int = ${ testExtrusion1 }
private def testExtrusion1(using Quotes): Expr[Int] =
  var extruded: Expr[Int] = null
  '{ (x: Int) =>
    ${
      extruded = '{x}
      extruded
    }
  }
  extruded

inline def test2(): Int = ${ testExtrusion2 }
private def testExtrusion2(using Quotes): Expr[Int] =
  '{ 1 +
    ${ var extruded: Expr[Int] = null; '{ (y: Int) => ${ extruded = '{y}; extruded } }; extruded }
  }

inline def test3(): Int = ${ testExtrusion3 }
private def testExtrusion3(using Quotes): Expr[Int] = {
  var extruded: Expr[Int] = null
  for i <- 1 to 3 do
    '{ (x: Int) =>
      ${
        if extruded == null then
            extruded = '{x}
        extruded
      }
    }
  extruded
}
