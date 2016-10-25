import scala.annotation.tailrec

object ListExercise {

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

  def foldLeft[A,B]: (List[A], B) => ((B,A) => B) => B = 
    (xs, z) => f => {
      
      @tailrec
      def iter(ys: List[A], acc: B): B = ys match {
        case Nil	  => acc
        case Cons(w, ws)  => iter(ws, f(acc, w)) 
      }

      iter(xs, z)
    } 
   
  def foldRight[A,B]: (List[A], B) => ((B, A) => B) => B =
    (xs, z) => f => foldLeft(reverse(xs), z)(f)

  def length[A]: List[A] => Int =
    xs => foldRight(xs, 0) { (t, _) => t + 1 }

  def reverse[A]: List[A] => List[A] =
    xs => foldLeft(xs, Nil: List[A]) { (ys,y) => Cons(y, ys) }

  def appendLeft[A]: (List[A], A) => List[A] =
    (xs, x) => foldLeft(reverse(xs), Cons(x, Nil)) { (ys, y) => Cons(y, ys) }

  def appendRight[A]: (List[A], A) => List[A] =
    (xs, x) => foldRight(xs, Cons(x, Nil)) { (ys, y) => Cons(y, ys) }

  def append[A]: (List[A], A) => List[A] = appendLeft

  def appendList[A]: (List[A], List[A]) => List[A] =
    (xs, ys) => foldRight(xs, ys) { (zs, z) => Cons(z, zs) }
  
  def flatten[A]: List[List[A]] => List[A] =
    xs => foldLeft(xs, Nil: List[A])(appendList)
  
  def map[A, B]: List[A] => (A => B) => List[B] =
    xs => f => foldRight(xs, Nil: List[B]) { (ys, y) => Cons(f(y), ys) }

  def filter[A]: List[A] => (A => Boolean) => List[A] =
    xs => p => foldRight(xs, Nil: List[A]) { (ys, y) => if(p(y)) Cons(y, ys) else ys }

  def flatMap[A, B]: List[A] => (A => List[B]) => List[B] =
    xs => f => flatten(map(xs)(f))

  def filter2[A]: List[A] => (A => Boolean) => List[A] =
    xs => p => flatMap(xs){ x => if(p(x)) Cons(x, Nil) else Nil }

  def zipWith[A, B]: (List[A], List[A]) => ((A, A) => B) => List[B] =
    (xs, ys) => f => {     
      @tailrec
      def iter(l1: List[A], l2: List[A], acc: List[B]): List[B] = (l1, l2) match {
        case (Nil, _) | (_, Nil)          => acc
        case (Cons(h1, t1), Cons(h2, t2)) => iter(t1, t2, Cons(f(h1, h2), acc))
      }

      reverse(iter(xs, ys, Nil))
    }

  def tail[A]: List[A] => List[A] =
    xs => xs match {
      case Nil          => Nil
      case Cons(y, ys)  => ys
    }

  @tailrec
  def startsWith[A](xs: List[A], sub: List[A]): Boolean = (xs, sub) match {
    case (_, Nil)                                   => true
    case (Cons(h1, t1), Cons(h2, t2)) if(h1 == h2)  => startsWith(t1, t2)
    case _                                          => false
  }

  @tailrec
  def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean = xs match {
    case Nil        => false
    case Cons(h, t) => if(startsWith(xs, sub)) true else hasSubsequence(t, sub)
  }


  ////////////////////////////////
  // Operations on lists of Int //
  ////////////////////////////////
  
  lazy val sum: List[Int] => Int =
    ns => foldRight(ns, 0)(_ + _)

  lazy val prod: List[Double] => Double =
    ns => foldRight(ns, 1d)(_ * _)

  lazy val addLists: (List[Int], List[Int]) => List[Int] =
    (xs, ys) => {
      
      @tailrec
      def iter(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = (l1, l2) match {
        case (Nil, _) | (_, Nil)          => acc
        case (Cons(h1, t1), Cons(h2, t2)) => iter(t1, t2, Cons(h1 + h2, acc))
      }

      reverse(iter(xs, ys, Nil))
    }
}
