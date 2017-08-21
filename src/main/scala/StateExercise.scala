object StateExercise {

  sealed trait RNG {
    def nextInt: (Int, RNG)
  }

  final case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt: RNG => (Int, RNG) =
    rng => {
      lazy val (i, s) = rng.nextInt
      if(i >= 0) (i, s) else (-(i + 1), s)
    }

  def double: RNG => (Double, RNG) =
    rng => {
      lazy val (n, s) = nonNegativeInt(rng)
      (n.toDouble / Int.MaxValue.toDouble, s)
    }

  def intDouble: RNG => ((Int,Double), RNG) =
    rng => {
      lazy val (i, s)  = rng.nextInt
      lazy val (d, s1) = double(s)
      ((i, d), s1)
    }
  
  def doubleInt: RNG => ((Double,Int), RNG) =
    rng => {
      lazy val ((i, d), s) = intDouble(rng)
      ((d, i), s)
    }
  
  def double3: RNG => ((Double,Double,Double), RNG) =
    rng => {
      lazy val (d1, s)  = double(rng)
      lazy val (d2, s1) = double(s)
      lazy val (d3, s2) = double(s1)
      ((d1, d2, d3), s2)
    }

  def ints: Int => RNG => (List[Int], RNG) =
    count => rng => (1 to count).foldLeft((Nil, rng): (List[Int], RNG)) {
      (c, _) => 
        val (n, s1) = c._2.nextInt
        (n :: c._1, s1)
    }

  type Rand[+A] = RNG => (A, RNG)
  
  val int: Rand[Int] = 
    _.nextInt

  def unit[A]: A => Rand[A] = 
    a => rng => (a, rng)

  def _map[A,B]: Rand[A] => (A => B) => Rand[B] =
    ra => f => rng => {
      lazy val (x, s1) = ra(rng)
      (f(x), s1)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt) { x => x - x % 2 }

  def double2: Rand[Double] =
    map(nonNegativeInt) { _.toDouble / Int.MaxValue.toDouble }

  def _map2[A,B,C]: Rand[A] => Rand[B] => ((A, B) => C) => Rand[C] =
    ra => rb => f => rng => {
      lazy val (a, s)  = ra(rng)
      lazy val (b, s1) = rb(s)
      (f(a, b), s1)
    }

  def both[A,B]: Rand[A] => Rand[B] => Rand[(A,B)] =
    ra => rb => map2(ra)(rb){ (_, _) }

  def intDouble2: Rand[(Int,Double)] =
    both(int)(double2)

  def doubleInt2: Rand[(Double,Int)] =
    both(double2)(int)

  def sequence[A]: List[Rand[A]] => Rand[List[A]] =
    _.foldLeft(unit(Nil): Rand[List[A]]) { 
      (xs, n) => map2(n)(xs) { _ :: _ }
    }

  def rngs[A]: Int => (=> Rand[A]) => Rand[List[A]] =
    count => (sequence compose List.fill[Rand[A]](count))

  def ints2: Int => Rand[List[Int]] =
    count => rngs[Int](count)(int)

  def flatMap[A, B]: Rand[A] => (A => Rand[B]) => Rand[B] =
    ra => f => rng => {
      lazy val (x, s) = ra(rng)
      f(x)(s)
    }

  def nonNegativeLessThan: Int => Rand[Int] =
    n => flatMap(nonNegativeInt) {
      x => 
        lazy val mod = x % n
        if(x + (n -1) - mod > 0) unit(mod) else nonNegativeLessThan(n)
    }

  def map[A, B]: Rand[A] => (A => B) => Rand[B] =
    ra => f => flatMap(ra){ a => unit(f(a)) }

  def map2[A, B, C]: Rand[A] => Rand[B] => ((A, B) => C) => Rand[C] =
    ra => rb => f => flatMap(ra){ a => map(rb) { b => f(a, b) } }

  final case class State[+A, S](run: S => (A, S)) {
    
    def map[B]: (A => B) => State[B, S] = 
      f => flatMap { a => (State.unit compose f)(a) } 

    def map2[B, C]: State[B, S] => ((A, B) => C) => State[C, S] =
      sb => f => flatMap { a => sb map { b => f(a, b) } }

    def flatMap[B]: (A => State[B, S]) => State[B, S] =
      f => State {
        s => 
          lazy val (a, s1) = run(s)
          f(a).run(s1)
      }
  }

  object State {

    def unit[A, S]: A => State[A, S] = 
      a => State(s => (a, s))

    def sequence[A, S]: List[State[A, S]] => State[List[A], S] =
      xs => xs.foldRight(unit[List[A], S](Nil)) {
        (n, s) => n.map2(s) { _ :: _ }
      }

    def sequenceTailRec[A, S]: List[State[A, S]] => State[List[A], S] =
      xs => xs.reverse.foldLeft(unit[List[A], S](Nil)) {
        (s, n) => n.map2(s) { _ :: _ }
      }

    type Rand[A] = State[RNG, A]
  }
}
