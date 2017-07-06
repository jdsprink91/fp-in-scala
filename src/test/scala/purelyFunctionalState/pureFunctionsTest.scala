package purelyFunctionalState;

import org.scalatest.FunSuite
import purelyFunctionalState.RNG
import purelyFunctionalState.RNG.Rand

class pureFunctionsTest extends FunSuite {
  val rng = RNG.simple(5)

  test("positive integer") {
    val (foo, rng2) = RNG.positiveInt(rng)
    val (bar, rng3) = RNG.positiveInt(rng2)
    val (baz, rng4) = RNG.positiveInt(rng3)
    assert(foo > 0)
    assert(bar > 0)
    assert(baz > 0)
  }

  test("double between 0 and 1") {
    val (foo, rng2) = RNG.double(rng)
    val (bar, rng3) = RNG.double(rng2)
    val (baz, rng4) = RNG.double(rng3)
    assert(foo >= 0 && foo < 1)
    assert(bar >= 0 && bar < 1)
    assert(baz >= 0 && baz < 1)
  }

  test("ints") {
    val (foo, rng2) = RNG.ints(5)(rng)
    assert(foo.length == 5)
  }

  test("positive integer rand") {
    val foo: Rand[Int] = RNG.positiveMax(5)
    val bar: Rand[Int] = RNG.positiveMax(500)
    val baz: Rand[Int] = RNG.positiveMax(5000000)

    val (fooNum, rng2) = foo(rng)
    val (barNum, rng3) = bar(rng2)
    val (bazNum, rng4) = baz(rng3)

    assert(fooNum >= 0 && fooNum <= 5)
    assert(barNum >= 0 && barNum <= 500)
    assert(bazNum >= 0 && bazNum <= 5000000)
  }

  test("double between 0 and 1 with rand") {
    val (foo, rng2) = RNG.randDouble(rng)
    val (bar, rng3) = RNG.randDouble(rng2)
    val (baz, rng4) = RNG.randDouble(rng3)
    assert(foo >= 0 && foo < 1)
    assert(bar >= 0 && bar < 1)
    assert(baz >= 0 && baz < 1)
  }
}
