package scala.reflect

import language.experimental.captureChecking

/** A class that implements structural selections using Java reflection.
 *
 *  It can be used as a supertrait of a class or be made available
 *  as an implicit conversion via `reflectiveSelectable`.
 *
 *  In Scala.js, it is implemented using a separate Scala.js-specific
 *  mechanism, since Java reflection is not available.
 */
trait Selectable extends scala.Selectable:

  /** The value from which structural members are selected.
   *  By default this is the Selectable instance itself, but it can
   *  be overridden.
   */
  protected def selectedValue: Any = this

  // The Scala.js codegen relies on this method being final for correctness
  /** Selects member with given name.
   *
   *  @param name the name of the structural member (field or no-arg method) to select
   */
  final def selectDynamic(name: String): Any =
    val rcls = selectedValue.getClass
    try
      val fld = rcls.getField(NameTransformer.encode(name)).nn
      ensureAccessible(fld)
      fld.get(selectedValue)
    catch case ex: NoSuchFieldException =>
      applyDynamic(name)()

  // The Scala.js codegen relies on this method being final for correctness
  /** Selects method and applies to arguments.
   *  @param name       the name of the selected method
   *  @param paramTypes the `Class` instances representing the selected method's formal parameter types
   *  @param args       the arguments to pass to the selected method
   */
  final def applyDynamic(name: String, paramTypes: Class[?]*)(args: Any*): Any =
    val rcls = selectedValue.getClass
    val mth = rcls.getMethod(NameTransformer.encode(name), paramTypes*).nn
    ensureAccessible(mth)
    mth.invoke(selectedValue, args.asInstanceOf[Seq[AnyRef]]*)

object Selectable:

  /** An implicit conversion that turns a value into a Selectable
   *  such that structural selections are performed on that value.
   *
   *  @param x the value to wrap in a `Selectable` for reflection-based structural member access
   */
  implicit def reflectiveSelectable(x: Any): Selectable =
    new DefaultSelectable(x)

  @inline // important for Scala.js
  private final class DefaultSelectable(override protected val selectedValue: Any) extends Selectable
end Selectable
