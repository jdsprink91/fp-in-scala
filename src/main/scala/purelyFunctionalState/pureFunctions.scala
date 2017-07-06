package purelyFunctionalState

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)
  case class State[S,+A](run: S => (A,S))

  def get[S]: State[S, S] = State(s => (s,s))
  def set[S, A](s: S, a: A): State[S, A] = State(s => (a, s))

  val int: Rand[Int] = _.nextInt

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                  ((1L << 48) - 1)
                  ((seed2 >>> 16).asInstanceOf[Int],
                  simple(seed2))
    }
  }

  def positiveInt(rng : RNG): (Int, RNG) = {
    val (num, rng2) = rng.nextInt
    val posNum = if (num == Int.MinValue) Int.MaxValue else num.abs
    (posNum, rng2)
  }

  def double(rng : RNG) : (Double, RNG) = {
    val (num, rng2) = RNG.positiveInt(rng)
    val dubNum = num.toDouble
    (dubNum / Double.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (num, rng2) = rng.nextInt
    val (num2, rng3) = rng2.nextInt
    ((num, num2.toDouble), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (num, rng2) = rng.nextInt
    val (num2, rng3) = rng2.nextInt
    ((num.toDouble, num2), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (num, rng2) = rng.nextInt
    val (num2, rng3) = rng2.nextInt
    val (num3, rng4) = rng3.nextInt
    ((num.toDouble, num2.toDouble, num3.toDouble), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    // if(count <= 0) {
    //   (Nil, rng)
    // }
    // else {
    //   val (num, rng2) = rng.nextInt
    //   val (list, rng3) = ints(count - 1)(rng2)
    //   (num :: list, rng3)
    // }
    sequence(List.tabulate(count)(unit[Int]))(rng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[S,A,B](s: S => (A, S))(f: A => B): S => (B, S) = {
    // rng => {
    //   val (a, rng2) = s(rng)
    //   (f(a), rng2)
    // }
    flatMap(s)(x => s => (f(x), s))
  }

  def positiveMax(n : Int): Rand[Int] = {
    map(int)(x => (x.abs / Int.MaxValue) * n)
  }

  def randDouble : Rand[Double] = {
    map(positiveInt)(_.toDouble.abs / Double.MaxValue)
  }

  def map2[S,A,B,C](ra: S => (A, S), rb: S => (B, S))(f: (A, B) => C): S => (C, S) = {
    // rng => {
    //   val (a, rng2) = ra(rng)
    //   val (b, rng3) = rb(rng2)
    //   (f(a, b), rng3)
    // }
    flatMap(ra)(x => s => {
      val (b, s2) = rb(s)
      (f(x, b), s2)
    })
  }

  def mapIntDouble(rng : RNG): Rand[(Int,Double)] = {
    map2(int, int)((x, y) => (x, y.toDouble))
  }

  def mapDoubleInt(rng : RNG): Rand[(Double, Int)] = {
    map2(int, int)((x, y) => (x.toDouble, y))
  }

  def sequence[A](fs: List[Rand[A]]) : Rand[List[A]] = {
    fs match {
      case Nil => rng => (Nil, rng)
      case(h :: t) => flatMap(h)(x => rng => {
        val (y, rng2) = sequence(t)(rng)
        (x :: y, rng2)
      })
    }
  }

  def flatMap[S,A,B](f: S => (A, S))(g: A => S => (B, S)): S => (B, S) = {
    s => {
      val (a, s2) = f(s)
      g(a)(s2)
    }
  }
}
