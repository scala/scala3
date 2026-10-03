//>using options -language:experimental.specializedTraits

trait Base[T]:
  def get: T

inline trait Faster[T: Specialized](x: T) extends Base[T]:
  override def get: T = x

object Base:
  def apply[T](x: T): Base[T] = new Base[T] {
    def get: T = x
  }
  inline def apply[T: Specialized](x: T): Faster[T] = new Faster(x) {}
  
  def main = 
    val faster = apply(42)