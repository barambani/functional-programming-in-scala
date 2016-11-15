object EitherExercise {

  sealed trait Either[+E, +A] extends Product with Serializable {

    def map[B >: A]: (A => B) => Either[E, B] = ???
    def flatMap[EE >: E, B]: (A => Either[EE, B]) => Either[EE, B] = ???
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???
    def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
  }

  final case class Left[+E](e: E) extends Either[E, Nothing]
  final case class Right[+A](a: A) extends Either[Nothing, A]
}
