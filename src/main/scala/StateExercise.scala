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

  def map[A,B]: Rand[A] => (A => B) => Rand[B] =
    s => f => rng => {
      lazy val (x, s1) = s(rng)
      (f(x), s1)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt) { x => x - x % 2 }

  def double2: Rand[Double] =
    map(nonNegativeInt) { _.toDouble / Int.MaxValue.toDouble }

  def map2[A,B,C]: Rand[A] => Rand[B] => ((A, B) => C) => Rand[C] =
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
}
