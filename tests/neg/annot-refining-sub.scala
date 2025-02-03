import compiletime.ops.int.+

class annot1(a: Any) extends scala.annotation.RefiningAnnotation
class annot2(a: Any, b: Any) extends scala.annotation.RefiningAnnotation
class annot3(a: Any, b: Any = 3) extends scala.annotation.RefiningAnnotation
class annot4[Int] extends scala.annotation.RefiningAnnotation

class Box[T](val a: Int)
case class Box2[T](val a: Int)
class Box3:
  type T

def id[T](x: T): T = x

type BoxAlias = Box[Int]
type Box2Alias = Box2[Int]

object O:
  val d: Int = 42

def main =
  val c: Int = 42
  val o: O.type = O

  val v1: Int @annot1(1) = ??? : Int @annot1(1) // error: fixme (constants are equal)
  val v2: Int @annot1(c) = ??? : Int @annot1(c)
  val v3: Int @annot1(O.d) = ??? : Int @annot1(O.d)
  val v4: Int @annot1(O.d) = ??? : Int @annot1(o.d) // error: fixme?
  val v5: Int @annot1((1, 2)) = ??? : Int @annot1((1, 2)) // error: fixme
  val v6: Int @annot1(1 + 2) = ??? : Int @annot1(1 + 2) // error: fixme
  val v7: Int @annot1(1 + 2) = ??? : Int @annot1(2 + 1) // error: fixme? should constant fold?
  val v8: Int @annot1(1 + c) = ??? : Int @annot1(1 + c) // error: fixme
  val v9: Int @annot1(1 + c) = ??? : Int @annot1(c + 1) // error (no algebraic normalization)
  val v10: Int @annot1(Box(1)) = ??? : Int @annot1(Box(1)) // error: fixme
  val v11: Int @annot1(Box(c)) = ??? : Int @annot1(Box(c))
  val v12: Int @annot1(Box2(1)) = ??? : Int @annot1(Box2(1)) // error: fixme
  val v13: Int @annot1(Box2(c)) = ??? : Int @annot1(Box2(c))
  val v14: Int @annot1(c: Int) = ??? : Int @annot1(c: Int)
  val v15: Int @annot1(c) = ??? : Int @annot1(c: Int) // error
  val v16: Int @annot1(c: Int) = ??? : Int @annot1(c) // error
  val v17: Int @annot1(id[Int]) = ??? : Int @annot1(id[Int])
  val v18: Int @annot1(id[Int]) = ??? : Int @annot1(id[String]) // error
  val v19: Int @annot1(id[Any]) = ??? : Int @annot1(id[Int]) // error
  val v20: Int @annot1(Box(1)) = ??? : Int @annot1(Box(1): BoxAlias) // error
  val v21: Int @annot1(Box(c): BoxAlias) = ??? : Int @annot1(Box(c)) // error
  val v22: Int @annot1(Box2(1)) = ??? : Int @annot1(Box2(1): Box2Alias) // error
  val v23: Int @annot1(Box2(c): Box2Alias) = ??? : Int @annot1(Box2(c)) // error
  val v24: Int @annot1(Box3()) = ??? : Int @annot1(Box3())
  val v25: Int @annot1(??? : Box3 {type T = Int}) = ??? : Int @annot1(??? : Box3 {type T = Int})
  val v26: Int @annot1(??? : Box3 {type T = Int}) = ??? : Int @annot1(??? : Box3 {type T = String}) // error
  val v27: Int @annot1(??? : Box3 {type T = Int}) = ??? : Int @annot1(??? : Box3) // error
  val v28: Int @annot1(a=c) = ??? : Int @annot1(a=c)
  val v29: Int @annot1(a=c) = ??? : Int @annot1(c) // error: fixme (same arguments, named vs positional)
  val v30: Int @annot1(c) = ??? : Int @annot1(a=c) // error: fixme
  val v31: Int @annot1((d: Int) => d) = ??? : Int @annot1((d: Int) => d)
  val v32: Int @annot1((d: Int) => d) = ??? : Int @annot1((e: Int) => e) // error: fixme (alpha equivalence)
  val v33: Int @annot1((e: Int) => e) = ??? : Int @annot1((d: Int) => d) // error: fixme
  val v34: Int @annot1((d: Int) => d + 1) = ??? : Int @annot1((e: Int) => e + 1) // error: fixme
  val v35: Int @annot1((d: Int) => d + 1) = ??? : Int @annot1((e: Int) => e + 1) // error: fixme
  val v36: Int @annot1((d: Int) => id[d.type]) = ??? : Int @annot1((e: Int) => id[e.type]) // error: fixme
  val v37: Int @annot1((d: Box3) => id[d.T]) = ??? : Int @annot1((e: Box3) => id[e.T]) // error: fixme
  val v38: Int @annot1((d: Int) => (d: Int) => d) = ??? : Int @annot1((e: Int) => (e: Int) => e) // error: fixme
  val v39: Int @annot1((d: Int) => ((e: Int) => d)(2)) = ??? : Int @annot1((e: Int) => ((e: Int) => e)(2)) // error: fixme

  val v40: Int @annot2(1, 2) = ??? : Int @annot2(1, 2) // error: fixme
  val v41: Int @annot2(c, c) = ??? : Int @annot2(c, c)
  val v42: Int @annot2(c, c) = ??? : Int @annot2(a=c, b=c) // error: fixme
  val v43: Int @annot2(a=c, c) = ??? : Int @annot2(c, b=c) // error: fixme
  val v44: Int @annot2(a=c, b=c) = ??? : Int @annot2(c, c) // error: fixme

  val v45: Int @annot3(1) = ??? : Int @annot3(1) // error: fixme
  val v46: Int @annot3(c) = ??? : Int @annot3(c)
  val v47: Int @annot3(1) = ??? : Int @annot3(1, 3) // error: fixme
  val v48: Int @annot3(1, 3) = ??? : Int @annot3(1) // error: fixme
  val v49: Int @annot3(c) = ??? : Int @annot3(c, 3) // error: fixme
  val v50: Int @annot3(c, 3) = ??? : Int @annot3(c) // error: fixme

  val v51: Int @annot4[1] = ??? : Int @annot4[1] // error: fixme
  val v52: Int @annot4[c.type] = ??? : Int @annot4[c.type]
  val v53: Int @annot4[O.d.type] = ??? : Int @annot4[O.d.type]
  val v54: Int @annot4[O.d.type] = ??? : Int @annot4[o.d.type]// error: fixme?
  val v55: Int @annot4[Int] = ??? : Int @annot4[Int]
  val v56: Int @annot4[Int] = ??? : Int @annot4[1] // error
  val v57: Int @annot4[(1, 2)] = ??? : Int @annot4[(1, 2)] // error: fixme
  val v58: Int @annot4[1 + 2] = ??? : Int @annot4[1 + 2] // error: fixme
  val v59: Int @annot4[1 + 2] = ??? : Int @annot4[2 + 1] // error: fixme
  val v60: Int @annot4[1 + c.type] = ??? : Int @annot4[1 + c.type] // error: fixme
  val v61: Int @annot4[1 + c.type] = ??? : Int @annot4[c.type + 1] // error
  val v62: Int @annot4[Box[Int]] = ??? : Int @annot4[Box[Int]]
  val v63: Int @annot4[Box[String]] = ??? : Int @annot4[Box[Int]] // error
  val v64: Int @annot4[Box2[Int]] = ??? : Int @annot4[Box2[Int]]
  val v65: Int @annot4[Box2[String]] = ??? : Int @annot4[Box2[Int]] // error
  val v66: Int @annot4[1] = ??? : Int @annot4[Int] // error
