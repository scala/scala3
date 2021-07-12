object Nats {
  trait Fold {
    trait Nat
  }
  type Inc = Fold {
    type Apply[N <: Nat] = Succ // error
  }
}