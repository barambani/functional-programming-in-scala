object TreeExercise {

  sealed trait Tree[+A] extends Product with Serializable
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A]: Tree[A] => Int = 
    t => t match {
      case Leaf(_)      => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  
  def max: Tree[Int] => Int = 
    t => t match {
      case Leaf(v)      => v
      case Branch(l, r) => max(l) max max(r)
    }

  def depth[A]: Tree[A] => Int = 
    t => t match {
      case Leaf(_)      => 1
      case Branch(l, r) => (depth(l) max depth(r)) + 1
    }

  def map[A, B]: Tree[A] => (A => B) => Tree[B] =
    t => f => t match {
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  def fold[A, B]: Tree[A] => (A => B) => ((B, B) => B) => B =
    t => f => m => t match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => m(fold(l)(f)(m), fold(r)(f)(m))
    }

  def sizeF[A]: Tree[A] => Int = 
    t => fold(t)(_ => 1)(1 + _ + _)

  def maxF: Tree[Int] => Int =
    t => fold(t)(v => v)(_ max _)

  def depthF[A]:  Tree[A] => Int = 
    t => fold(t)(_ => 1){ (l, r) => (l max r) + 1 }

  def mapF[A, B]: Tree[A] => (A => B) => Tree[B] =
    t => f => fold[A, Tree[B]](t)(v => Leaf(f(v)))(Branch(_, _))
}
