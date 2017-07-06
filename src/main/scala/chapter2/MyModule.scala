package chapter2

/* Another comment */
/** A documentation comment */
object MyModule {
  // book code
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // exercise code
  def fib(n: Int): Int = {
    def go(prev: Int, curr: Int, iter: Int): Int =
      if (iter <= 0) prev
      else go(curr, prev + curr, iter - 1)
    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def go(as: Array[A], prev : Int, curr : Int, sorted : Boolean) : Boolean = {
      if(curr >= as.length) sorted
      else go(as, curr, curr + 1, sorted && gt(as(prev), as(curr)))
    }
    go(as, 0, 1, true)
  }

  def partiall[A, B, C](a : A, f : (A, B) => C) : B => C = {
    def go(b : B) : C = {
      f(a, b)
    }
    go
  }

  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    def go(a : A) : B => C = {
      def foo(b : B) : C = {
        f(a, b)
      }
      foo
    }
    go
  }

  def compose[A, B, C](f: B => C, g: A => B) : A => C = {
    def go(a : A) : C = {
      f(g(a))
    }
    go
  }
}
