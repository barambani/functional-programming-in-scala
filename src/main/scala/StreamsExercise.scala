import scala.annotation.tailrec

object StreamsExercise {

  final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  final case object Empty extends Stream[Nothing]

  sealed trait Stream[+A] {

    import Stream._
  
    def headOption: Option[A] =
      this match {
        case Empty        => None
        case Cons(fh, _)  => Some(fh())
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

    def notTailToList: List[A] =
      this match {
        case Empty      => Nil
        case Cons(h, t) => h() :: t().toList
      }

    def take: Int => Stream[A] = 
      n => this match {
        case Cons(fh, ft) if n > 0  => cons(fh(), ft().take(n - 1))
        case Cons(fh, _) if n == 1  => cons(fh(), empty)
        case _                      => empty
      }

    def drop: Int => Stream[A] = 
      n => this match {
        case Cons(_, ft) if n > 0   => ft().drop(n - 1)
        case _                      => this
      }

    def takeWhile: (A => Boolean) => Stream[A] =
      p => this match {
        case Cons(fh, ft) if p(fh())  => cons(fh(), ft() takeWhile p)
        case _                        => empty
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
