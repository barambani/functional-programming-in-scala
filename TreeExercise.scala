import scala.annotation.tailrec

object TreeExercise {

  sealed trait Tree[+A] extends Product with Serializable
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def foldLeft[A, B](tree: Tree[A], z: B)(implicit f: (B, A) => B): B = {

    def iter(t: Tree[A])(acc: B): B = t match {
      case Leaf(v)      => f(acc, v)
      case Branch(l, r) => (iter(r) _ compose iter(l))(acc)
    }

    iter(tree)(z)
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }
  
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(v)      => v
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }
}
