object OptionExercise {

  trait Option[+A] extends Product with Serializable {

    def map[B]: (A => B) => Option[B] = 
      f => this match {
        case Some(v)  => Some(f(v))
        case None     => None
      }

    def flatMap[B]: (A => Option[B]) => Option[B] =
      f => map(f) getOrElse None

    def getOrElse[B >: A]: B => B =
      d => this match {
        case Some(v)  => v
        case None     => d
      }
    
    def orElse[B >: A](ob: => Option[B]): Option[B] = ???
    def filter: (A => Boolean) => Option[A] = ???
  }

  final case class Some[+A](v: A) extends Option[A]
  final case object None extends Option[Nothing]
}