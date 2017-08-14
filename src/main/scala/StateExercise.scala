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
}
