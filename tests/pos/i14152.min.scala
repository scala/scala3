val aa1 = {
  object O1 extends AnyRef
  Array(O1)
}

val aa2: Array[_ <: AnyRef] = aa1
