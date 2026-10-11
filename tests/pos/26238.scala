object toplevel {

import util.ChainingOps.{*, given}

object S {

trait AllocatedNodeTypeFamily:
  type Node

trait Parser[+A] private[S] () :
  type SpcNd

type OutliningParser[+A, +Node]
= Parser[A] { type SpcNd <: Node }

extension [R1] (g1: Parser[R1] )
              (using pfmoIspcndi: ParserFlatMapOp.ISpcNdInvariant = new ParserFlatMapOp.ISpcNdInvariant {}.asInstanceOf[ParserFlatMapOp.ISpcNdInvariant { type SpcNdObj = g1.SpcNd } ] )

            def     map[R2] (m: R1 => (       R2  ) ): OutliningParser[R2, g1.SpcNd] = new Parser[Any] {}.asInstanceOf
            def flatMap[R2] (m: R1 => (Parser[R2] ) ): OutliningParser[R2, g1.SpcNd] = new Parser[Any] {}.asInstanceOf

object ParserFlatMapOp {

  given given_ISpcNdInvariant_PModAllocatedNodeTypeFamily
  : (x: AllocatedNodeTypeFamily ) => (ISpcNdInvariant { type SpcNdObj = x.Node })
  = new ISpcNdInvariant {}.asInstanceOf

  trait ISpcNdInvariant :
    type SpcNdObj

} // ParserFlatMapOp.

def defaultIntParser
  : Parser[Int]
= new Parser[Any] {}.asInstanceOf

} // S.

locally :
  locally :
    for
      r1 <- S.defaultIntParser
      r2 <- S.defaultIntParser
    yield { 5 }
  .toString()

}
