object OptionExercise {

  trait Option[+A] extends Product with Serializable {

    def map[B]: (A => B) => Option[B] = 
      f => this match {
        case Some(v)  => Some(f(v))
        case None     => None
      }

    def flatMap[B]: (A => Option[B]) => Option[B] =
      f => map(f) getOrElse None

    def getOrElse[B >: A](d: => B): B =
      this match {
        case Some(v)  => v
        case None     => d
      }
    
    def orElse[B >: A](ob: => Option[B]): Option[B] =
      map(_ => this) getOrElse ob

    def filter: (A => Boolean) => Option[A] =
      p => flatMap {
        a => if(p(a)) this else None
      }
  }

  object Option {

    def map2[A, B, C]: (Option[A], Option[B]) => ((A, B) => C) => Option[C] =
      (ma, mb) => f => ma flatMap (a => mb map (b => f(a, b)))

    def sequence[A]: List[Option[A]] => Option[List[A]] =
      xs => xs.foldRight[Option[List[A]]](Some(Nil)){ (x, ys) => map2(x, ys)(_ :: _) }

    def traverse[A, B]: List[A] => (A => Option[B]) => Option[List[B]] =
      xs => f => xs.foldRight[Option[List[B]]](Some(Nil)){ (x, ys) => map2(f(x), ys)(_ :: _) }

    def sequenceT[A]: List[Option[A]] => Option[List[A]] =
      xs => traverse(xs)(ma => ma)
  }

  final case class Some[+A](v: A) extends Option[A]
  final case object None extends Option[Nothing]
}
