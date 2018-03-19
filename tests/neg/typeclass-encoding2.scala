// Used to crash with:
//   assertion failed: class <refinement> has non-class parent
// Once that was fixed, another crash with:
//   assertion failed: invalid prefix WildcardType(NoType)
object runtime1 {

  trait TypeClass1[A] {
    val common: TypeClassCommon1
    type This[X] = common.This[X]
  }

  trait TypeClassCommon1 { self =>
    type This[X]
    type Instance[X] <: TypeClass1[X]
    def inject[A](x: This[A]): Instance[A]// { val common: self.type }
  }

  trait Extension1[From[_], To[X] <: TypeClass1[X]] extends TypeClassCommon1 {
    type This[X] = From[X]
    type Instance[X] = To[X]
  }

  implicit def inject[A, From[_]](x: From[A])
      (implicit ev: Extension1[From, _]): ev.Instance[A] { type This[X] = From[X] } =
    ev.inject(x) // error: found: ev.To[A], required: ev.To[A]{This = From}
}