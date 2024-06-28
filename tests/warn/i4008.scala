

// ===== Template annotations =====


// class, 1TP, invalid ref
@annotation.implicitNotFound("An implicit ShouldWarn1[${B}] is not in scope") // warn
class ShouldWarn1[A]

// trait, 1TP, invalid ref
@annotation.implicitNotFound("An implicit ShouldWarn2[${A}] is not in scope") // warn
trait ShouldWarn2[B]

// trait, 2TP, 1 invalid ref
@annotation.implicitNotFound("An implicit ShouldWarn3[${A},${B}] is not in scope") // warn
trait ShouldWarn3[B, C]

// class, 2TP, 2 invalid refs
@annotation.implicitNotFound("An implicit ShouldWarn4[${A},${B}] is not in scope") // warn // warn
class ShouldWarn4[C, D]

// class, 2TP, 1 invalid multi-char refs
@annotation.implicitNotFound("An implicit ShouldWarn5[${C},${Abc}] is not in scope") // warn
class ShouldWarn5[C, D]

// trait, 1TP, valid ref
@annotation.implicitNotFound("An implicit ShouldntWarn1[${A}] is not in scope")
trait ShouldntWarn1[A]

// class, 2TP, only one ref but that one is valid
@annotation.implicitNotFound("An implicit ShouldntWarn2[${A}, ...] is not in scope")
class ShouldntWarn2[A, B]

// trait, 2TP, 2 valid refs
@annotation.implicitNotFound("An implicit ShouldntWarn3[${A}, ${B}] is not in scope")
trait ShouldntWarn3[A, B]

// class, 2TP, 2 valid refs
@annotation.implicitNotFound("An implicit ShouldntWarn4[${Hello},${World}] is not in scope")
class ShouldntWarn4[Hello, World]

// ===== DefDef param annotations =====


@annotation.implicitNotFound("Hopefully you don't see this!")
class C[A](using @annotation.implicitNotFound("No C[${B}] found") c: Class[A]) // warn

def someMethod1[A](using @annotation.implicitNotFound("No C[${B}] found") sc: C[A]) = 0 // warn

def someMethod2[A](using @annotation.implicitNotFound("No C[${A}] found") sc: C[A]) = ""