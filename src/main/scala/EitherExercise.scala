object EitherExercise {

  sealed trait Either[+E, +A] extends Product with Serializable {

    def map[B]: (A => B) => Either[E, B] = 
      f => this match {
        case Left(e)  => Left(e)
        case Right(a) => Right(f(a))
      }

    def flatMap[EE >: E, B]: (A => Either[EE, B]) => Either[EE, B] =
      f => this match {
        case Left(e)  => Left(e)
        case Right(a) => f(a)
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(_)  => b
        case Right(_) => this
      }

    def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this flatMap (ra => b map (rb => f(ra, rb)))
  }

  final case class Left[+E](e: E) extends Either[E, Nothing]
  final case class Right[+A](a: A) extends Either[Nothing, A]
  
  object Either {
  
    def sequence[E, A]: List[Either[E, A]] => Either[E, List[A]] =
      xs => traverse(xs)(x => x)

    def traverse[E, A, B]: List[A] => (A => Either[E, B]) => Either[E, List[B]] =
      xs => f => xs.foldLeft[Either[E, List[B]]](Right(Nil)) { 
        (ys, i) => f(i).map2(ys) { _ :: _ }
      }
  }
}
