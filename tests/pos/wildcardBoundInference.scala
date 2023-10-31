// Tests translated from scalac SI-6189 by @retronym

/*
public class Exist<T extends String> {
  // java helpfully re-interprets Exist<?> as Exist<? extends String>
  public Exist<?> foo() { throw new RuntimeException(); }
}
*/
class Exist[T <: String] {
  def foo: Exist[?] = null
}

/*
public class ExistF<T extends ExistF<T>> {
  // java helpfully re-interprets ExistF<?> as ExistF<?0 extends ExistF<?0>>
  public ExistF<?> foo() { throw new RuntimeException(); }
}
*/

class ExistF[T <: ExistF[T]] {
  def foo: ExistF[?] = null
}

/*
public class ExistIndir<T extends String, U extends T> {
  // java helpfully re-interprets ExistIndir<?> as ExistIndir<? extends String>
  public ExistIndir<?, ?> foo() { throw new RuntimeException(); }
}
*/

class ExistIndir[T <: String, U <: T] {
  def foo: ExistIndir[?, ?] = null
}

class Test {
  class MyExist extends ExistF[MyExist]
  // SI-8197, SI-6169: java infers the bounds of existentials, so we have to as well now that SI-1786 is fixed...
  def stringy: Exist[? <: String] = (new Exist[String]).foo
  // def fbounded: (ExistF[t] forSome {type t <: ExistF[t] }) = (new MyExist).foo
  def indir: ExistIndir[? <: String, ? <: String] = (new ExistIndir[String, String]).foo
}


/*
public abstract class OP<T> { }
public interface Skin<C extends Skinnable> { }
public interface Skinnable {
  OP<Skin<?>> skinProperty();
}
*/
class OP[T]
trait Skin[C <: Skinnable]
trait Skinnable {
  def skinProperty: OP[Skin[?]]
}
object ObjectProperty {
  implicit def jfxObjectProperty2sfx[T](p: OP[T]): ObjectProperty[T] = new ObjectProperty[T](p)
}

class ObjectProperty[T](val deleg: OP[T])

trait TestWildcardBoundInference {
  def deleg: Skinnable
  def skin: ObjectProperty[Skin[? /* inferred: <: Skinnable */]] = ObjectProperty.jfxObjectProperty2sfx(deleg.skinProperty)
  skin: ObjectProperty[Skin[?  <: Skinnable]]

  def skinCheckInference = deleg.skinProperty
  skinCheckInference: ObjectProperty[Skin[?  <: Skinnable]]
}
