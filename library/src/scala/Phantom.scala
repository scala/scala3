/* Defined synthetically
package scala

trait Phantom {
  /** Phantom.Any does not extend scala.Any */
  protected /*final*/ trait Any

  protected final abstract class Nothing extends Any

  protected final def assume[P >: this.Nothing <: this.Any]: P =
    null.asInstanceOf[P] // This implementation matches the erased implementation
}
*/
