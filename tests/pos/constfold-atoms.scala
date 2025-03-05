class annot() extends scala.annotation.StaticAnnotation
class refiningAnnot() extends scala.annotation.RefiningAnnotation

type Foo = 4

@main def main =
  val v1: 4 = 2 + 2
  val v2: 8 = v1 + v1

  val v3: 4 | v1.type = 4
  val v4: 8 = v3 + v3

  val v5: 4 & v1.type = 4
  val v6: 8 = v5 + v5

  val v7: 4 @annot = 4
  val v8: 8 = v7 + v7

  val v9: 4 @refiningAnnot = ???
  val v10: 8 = v9 + v9

  val v11: Foo = 4
  val v12: 8 = v11 + v11
