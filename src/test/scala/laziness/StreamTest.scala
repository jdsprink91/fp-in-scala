package laziness

import org.scalatest.FunSuite
import laziness.Stream

class StreamTest extends FunSuite {
  test("to list") {
    assert(Stream(1,2,3).toList == List(1,2,3))
  }

  test("take") {
    assert(Stream(1,2,3).take(2).toList == List(1,2))
    assert(Stream(1,2,3).take(5).toList == List(1,2,3))
  }

  test("take while") {
    assert(Stream(2,4,6,1,3,5,8,9).takeWhile(_ % 2 == 0).toList == List(2,4,6))
    assert(Stream(3,4,6,1,3,5,8,9).takeWhile(_ % 2 == 0).toList == List())
  }

  test("for all") {
    assert(Stream(2,4,6,8).forAll(_ % 2 == 0) == true)
    assert(Stream(2,4,3,6,8).forAll(_ % 2 == 0) == false)
  }

  test("map") {
    assert(Stream(1,2,3,4).map(_ + 10).toList == List(11,12,13,14))
  }

  test("filter") {
    assert(Stream(11,12,13,14).filter(_ % 2 == 0).toList == List(12,14))
  }

  test("append") {
    assert(Stream(1,2,3).append(Stream(10,11,12)).toList == List(1,2,3,10,11,12))
  }

  test("constant") {
    assert(Stream.constant(3).take(4).toList == List(3,3,3,3))
  }

  test("from") {
    assert(Stream.from(3).take(4).toList == List(3,4,5,6))
  }

  test("fibs") {
    assert(Stream.fibs().take(4).toList == List(0, 1, 1, 2))
    assert(Stream.fibs().take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("zip") {
    assert(Stream.zip(Stream(1,2,3), Stream(10,11,12))(_ + _).toList == List(11,13,15))
  }

  test("zip all") {
    assert(Stream.zipAll(Stream(1,2,3,4,5), Stream(10,11,12))(_ + _).toList == List(11,13,15,4,5))
  }
}
