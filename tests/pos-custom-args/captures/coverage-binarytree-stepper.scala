//> using options -Yexplicit-nulls -Wsafe-init

object RedBlackTree:
  final class Tree[K, +V](
      private val _value: AnyRef | Null,
      private val _left: Tree[K, ?] | Null,
      private val _right: Tree[K, ?] | Null):

    final def value = _value.asInstanceOf[V]
    final def left = _left.asInstanceOf[Tree[K, V] | Null]
    final def right = _right.asInstanceOf[Tree[K, V] | Null]

trait Stepper[+A]
trait EfficientSplit

object IntTreeStepper:
  def from[T](
      maxLength: Int,
      tree: T | Null,
      left: T => T | Null,
      right: T => T | Null,
      extract: T => Int): Stepper[Int] & EfficientSplit =
    new Stepper[Int] with EfficientSplit {}

final class TreeSteppers[K, V](tree: RedBlackTree.Tree[K, V] | Null):
  type T = RedBlackTree.Tree[K, V]

  def size: Int = 0

  def valueStepper: Stepper[Int] & EfficientSplit =
    IntTreeStepper.from[T](
      size,
      tree,
      _.left,
      _ => null,
      _.value.asInstanceOf[Int])
