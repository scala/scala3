enum Tree[+A]:
  case Branch(left: Tree[A], right: Tree[A])
  case Leaf(value: A)

enum Context[+A]:
  case Empty
  case InLeft(right: Tree[A], parent: Context[A])
  case InRight(left: Tree[A], parent: Context[A])

import Tree.*, Context.*

class Zipper[+A](val focus: Tree[A], val context: Context[A]):
  def unfocus: Tree[A] = context match
    case Empty => focus
    case _ => moveUp.unfocus
  def moveUp: Zipper[A] = context match
    case Empty => this
    case InLeft(right, parent) => Zipper(Branch(focus, right), parent)
    case InRight(left, parent) => Zipper(Branch(left, focus), parent)
  def moveLeft: Zipper[A] = focus match
    case Leaf(_) => this
    case Branch(left, right) => Zipper(left, InLeft(right, context))
  def moveRight: Zipper[A] = focus match
    case Leaf(_) => this
    case Branch(left, right) => Zipper(right, InRight(left, context))
  def replaceFocus[B >: A](newFocus: Tree[B]): Zipper[B] =
    Zipper(newFocus, context)

extension[A](tree: Tree[A]) def focus: Zipper[A] = Zipper(tree, Empty)
