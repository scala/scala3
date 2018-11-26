trait A {
  type A_This <: A
}
trait B extends A {
  type A_This = B_This
  type B_This <: B
}
trait C extends A {
  type A_This = C_This
  type C_This <: C
}
trait D extends B, C {
  type B_This = D_This
  type C_This = D_This
  type D_This <: D
}