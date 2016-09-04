object ListExample {

   object List {

      def apply[A](as: A*): List[A] =
         if(as.isEmpty) Nil
	 else Cons(as.head, apply(as.tail: _*))

   }

   sealed trait List[+A]
   case object Nil extends List[Nothing]
   case class Cons[A](x: A, xs: List[A]) extends List[A]


   def foldRightBroken[A,B](as: List[A], z: B)(implicit f: (A,B) => B): B =
      as match {
         case Nil		=> z
	 case Cons(x, xs)	=> f(x, foldRightBroken(xs, z))
      }

   def foldRight[A,B](xs: List[A], z: B)(f: (B, A) => B): B =
      foldLeft(reverse(xs), z)(f)

   def foldLeft[A,B](xs: List[A], z: B)(f: (B,A) => B): B = {
      def iter(ys: List[A], acc: B): B = ys match {
         case Nil		=> acc
	 case Cons(w, ws)	=> iter(ws, f(acc, w)) 
      }

      iter(xs, z)
   }

   def sum(ns: List[Int]): Int =
      foldRight(ns, 0)(_ + _)

   def prod(ns: List[Double]): Double =
      foldRight(ns, 1d)(_ * _)

   def length[A](xs: List[A]): Int = 
      foldRight(xs, 0){ (t, _) => t + 1 }

   def reverse[A](xs: List[A]): List[A] =
      foldLeft(xs, Nil: List[A]){ (ys,y) => Cons(y, ys) }
}

