object ListExample {

  object List {
    def apply[A](as: A*): List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  sealed trait List[+A] extends Product with Serializable
  final case object Nil extends List[Nothing]
  final case class Cons[A](x: A, xs: List[A]) extends List[A]

  def foldRightNotTailRec[A,B](as: List[A], z: B)(implicit f: (A,B) => B): B =
    as match {
      case Nil		=> z
      case Cons(x, xs)	=> f(x, foldRightNotTailRec(xs, z))
    }

  def foldRight[A,B]: (List[A], B) => ((B, A) => B) => B =
    (xs, z) => f => foldLeft(reverse(xs), z)(f)

  def foldLeft[A,B]: (List[A], B) => ((B,A) => B) => B = 
    (xs, z) => f => {
      def iter(ys: List[A], acc: B): B = ys match {
        case Nil	  => acc
        case Cons(w, ws)  => iter(ws, f(acc, w)) 
      }
      iter(xs, z)
    }

  def length[A]: List[A] => Int =
    xs => foldRight(xs, 0) { (t, _) => t + 1 }

  def reverse[A]: List[A] => List[A] =
    xs => foldLeft(xs, Nil: List[A]) { (ys,y) => Cons(y, ys) }

  def append[A]: (List[A], A) => List[A] = appendLeft

  def appendLeft[A]: (List[A], A) => List[A] =
    (xs, x) => foldLeft(reverse(xs), Cons(x, Nil)) { (ys, y) => Cons(y, ys) }

  def appendRight[A]: (List[A], A) => List[A] =
    (xs, x) => foldRight(xs, Cons(x, Nil)) { (ys, y) => Cons(y, ys) }

  lazy val sum: List[Int] => Int =
    ns => foldRight(ns, 0)(_ + _)

  lazy val prod: List[Double] => Double =
    ns => foldRight(ns, 1d)(_ * _)
}
