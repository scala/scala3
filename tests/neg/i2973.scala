import annotation.unchecked.uncheckedVariance

class InvClass[U]
trait InvTrait[U]
class ContraClass[-T]
trait ContraTrait[-T]
class CoClass[+T]
trait CoTrait[+T]

class Contra[-T] extends InvClass[T] // error: contravariant type T occurs in invariant position
class Co[+T] extends InvClass[T] // error

class ContraInvT[-T] extends InvTrait[T] // error
class ContraCoT[-T] extends CoTrait[T] // error
class CoInvT[+T] extends InvTrait[T] // error
class CoContraT[+T] extends ContraTrait[T] // error
class CoInvCoT[+T] extends InvTrait[T] with CoTrait[T] // error
class CoContraCoT[+T] extends ContraTrait[T] with CoTrait[T] // error

class ContraContraInv[-T] extends ContraClass[T] with InvTrait[T] // error
class ContraCoInv[-T] extends CoClass[T] with InvTrait[T] // error
class CoCoInv[+T] extends CoClass[T] with InvTrait[T] // error

class ContraInvContra[-T] extends InvClass[T] with ContraTrait[T] // error
class CoInvContra[+T] extends InvClass[T] with ContraTrait[T] // error
class ContraInvCo[-T] extends InvClass[T] with CoTrait[T] // error
class CoInvCo[+T] extends InvClass[T] with CoTrait[T] // error

// @uncheckedVariance but in the wrong place
class ContraContraInv2[-T] extends ContraClass[T] @uncheckedVariance with InvTrait[T] // error
class ContraCoInv2[-T] extends CoClass[T] @uncheckedVariance with InvTrait[T] // error

trait Prefix1 {
  trait Prefix2 {
    class InvClass[U]
    trait InvTrait[U]
    class ContraClass[-T]
    trait ContraTrait[-T]
    class CoClass[+T]
    trait CoTrait[+T]
  }
}

trait Prefix3 {
  def f(v1: Prefix1)(v2: v1.Prefix2): Unit = {
    class Contra[-T] extends InvClass[T] // error: contravariant type T occurs in invariant position
    class Co[+T] extends InvClass[T] // error

    class ContraInvT[-T] extends InvTrait[T] // error
    class ContraCoT[-T] extends CoTrait[T] // error
    class CoInvT[+T] extends InvTrait[T] // error
    class CoContraT[+T] extends ContraTrait[T] // error
    class CoInvCoT[+T] extends InvTrait[T] with CoTrait[T] // error
    class CoContraCoT[+T] extends ContraTrait[T] with CoTrait[T] // error

    class ContraContraInv[-T] extends ContraClass[T] with InvTrait[T] // error
    class ContraCoInv[-T] extends CoClass[T] with InvTrait[T] // error
    class CoCoInv[+T] extends CoClass[T] with InvTrait[T] // error

    class ContraInvContra[-T] extends InvClass[T] with ContraTrait[T] // error
    class CoInvContra[+T] extends InvClass[T] with ContraTrait[T] // error
    class ContraInvCo[-T] extends InvClass[T] with CoTrait[T] // error
    class CoInvCo[+T] extends InvClass[T] with CoTrait[T] // error

    class ContraContraInv2[-T] extends ContraClass[T] @uncheckedVariance with InvTrait[T] // error
    class ContraCoInv2[-T] extends CoClass[T] @uncheckedVariance with InvTrait[T] // error
  }
}
