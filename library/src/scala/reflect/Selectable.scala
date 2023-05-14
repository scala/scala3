package scala.reflect

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
  /** Select member with given name */
  final def selectDynamic(name: String): Any =
    val rcls = selectedValue.getClass
    try
      val fld = rcls.getField(name).nn
      ensureAccessible(fld)
      fld.get(selectedValue)
    catch case ex: NoSuchFieldException =>
      applyDynamic(name)()

  // The Scala.js codegen relies on this method being final for correctness
  /** Select method and apply to arguments.
   *  @param name       The name of the selected method
   *  @param paramTypes The class tags of the selected method's formal parameter types
   *  @param args       The arguments to pass to the selected method
   */
  final def applyDynamic(name: String, paramTypes: Class[_]*)(args: Any*): Any =
    val rcls = selectedValue.getClass
    val mth = rcls.getMethod(name, paramTypes: _*).nn
    ensureAccessible(mth)
    mth.invoke(selectedValue, args.asInstanceOf[Seq[AnyRef]]: _*)

object Selectable:

  /** An implicit conversion that turns a value into a Selectable
   *  such that structural selections are performed on that value.
   */
  implicit def reflectiveSelectable(x: Any): Selectable =
    new DefaultSelectable(x)

  @inline // important for Scala.js
  private final class DefaultSelectable(override protected val selectedValue: Any) extends Selectable
end Selectable
