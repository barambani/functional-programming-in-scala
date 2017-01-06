import scala.annotation.tailrec

object StreamsExercise {

  final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  final case object Empty extends Stream[Nothing]

  sealed trait Stream[+A] {
  
    def headOption: Option[A] =
      this match {
        case Empty      => None
        case Cons(fh, _) => Some(fh())
      }

    def tail: Stream[A] =
      this match {
        case Empty        => Empty
        case Cons(_, ft)  => ft()
      }

    def toList: List[A] = {
      @tailrec
      def loop(st: Stream[A], o: List[A]): List[A] = 
        st match {
          case Empty        => o
          case Cons(fh, ft) => loop(ft(), fh() :: o)
        }

      loop(this, Nil).reverse
    }

    def toListNotTail: List[A] =
      this match {
        case Empty      => Nil
        case Cons(h, t) => h() :: t().toList
      }
  }

  object Stream {
  
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head: A = h
      lazy val tail: Stream[A] = t

      Cons(() => head, () => tail)
    }

    def empty: Stream[Nothing] = Empty

    def apply[A](as: A*): Stream[A] =
      if(as.isEmpty)  empty
      else            cons(as.head, apply(as.tail: _*))
  }
}
