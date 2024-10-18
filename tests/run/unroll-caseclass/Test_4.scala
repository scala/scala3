//> using options -experimental
// scalajs: --skip

import example.*
// !! IMPORTANT: If you remove this test, also remove unroll-caseclass.check
@main def Test(): Unit = {
  println("=== Unrolled Test V1 ===")
  UnrollTestMainV1.main(Array.empty)
  println("=== Unrolled Test V2 ===")
  UnrollTestMainV2.main(Array.empty)
  println("=== Unrolled Test V3 ===")
  UnrollTestMainV3.main(Array.empty)
}
