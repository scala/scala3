trait FooBase {
  type This <: FooBase { type This <: FooBase.this.This } & FooBase { type This <: FooBase.this.This } // error
}