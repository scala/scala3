package woes

trait Txn[T <: Txn[T]]

trait Impl[Repr[~ <: Txn[~]]] {
  final type Ext = Extension[Repr]  // Huh!
}

trait Extension[Repr[~ <: Txn[~]]]