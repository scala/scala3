object Example {
  case class C[H, T](h: H, t: T)

  type I = Int

  val p
    : C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,C[I,I]]]]]]]]]]]]]]]]]]]]]]]
    = C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,C(1,1)))))))))))))))))))))))
}