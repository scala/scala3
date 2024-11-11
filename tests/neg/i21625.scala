trait LB[X]; trait UB[Y]

trait Foo[-L, +U]:

  type F_LA_LB[K >: L] >: LB[K] // error: >: L
  type F_NL_LB[M <: L] >: LB[M] // was: error: <: L
  type F_UA_LB[T >: U] >: LB[T] // was: error: >: U
  type F_NU_LB[V <: U] >: LB[V] // error: <: U

  type F_LA_UB[K >: L] <: UB[K] // error: >: L
  type F_NL_UB[M <: L] <: UB[M]
  type F_UA_UB[T >: U] <: UB[T]
  type F_NU_UB[V <: U] <: UB[V] // error: <: U

  def hk[
       F_LA_LB[K >: L] >: LB[K], // was: error: >: L
       F_NL_LB[M <: L] >: LB[M], // error: <: L
       F_UA_LB[T >: U] >: LB[T], // error: >: U
       F_NU_LB[V <: U] >: LB[V], // was: error: <: U

       F_LA_UB[K >: L] <: UB[K],
       F_NL_UB[M <: L] <: UB[M], // error: <: L
       F_UA_UB[T >: U] <: UB[T], // error: >: U
       F_NU_UB[V <: U] <: UB[V],
  ]: Unit

  type K >: L
  type M <: L // error: <: L
  type T >: U // error: >: U
  type V <: U

  def d_K[K >: L](): K // error: >: L
  def d_L():         L // error: => L
  def d_M[M <: L](): M
  def d_T[T >: U](): T
  def d_U():         U
  def d_V[V <: U](): V // error: <: U

  val v_K: K
  val v_L: L // error: L
  val v_M: M // indirect error: M <: L
  val v_T: T // indirect error: T >: U
  val v_U: U
  val v_V: V

  // reverse

  def d_K_Unit[K >: L](): K => Unit // error: >: L
  def d_L_Unit():         L => Unit
  def d_M_Unit[M <: L](): M => Unit
  def d_T_Unit[T >: U](): T => Unit
  def d_U_Unit():         U => Unit // error: U =>
  def d_V_Unit[V <: U](): V => Unit // error: <: U

  val v_K_Unit: K => Unit
  val v_L_Unit: L => Unit
  val v_M_Unit: M => Unit
  val v_T_Unit: T => Unit
  val v_U_Unit: U => Unit // error: U =>
  val v_V_Unit: V => Unit
