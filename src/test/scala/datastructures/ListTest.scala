package datastructures

import org.scalatest.FunSuite
import datastructures.List

class ListTest extends FunSuite {
  test("tail") {
    assert(List.tail(Nil) == Nil)
    assert(List.tail(List(1,2,3)) == List(2, 3))
  }

  test("drop") {
    assert(List.drop(Nil, 0) == Nil)
    assert(List.drop(Nil, 5) == Nil)
    assert(List.drop(List(1,2,3), 1) == List(2, 3))
    assert(List.drop(List(1,2,3), 2) == List(3))
  }

  test("drop while") {
    assert(List.dropWhile(List(1,2,3))(x => x < 2) == List(2, 3))
  }

  test("set head") {
    assert(List.setHead(List(1,2,3))(5) == List(5,2,3))
  }

  test("init") {
    assert(List.init(List(1,2,3,4)) == List(1,2,3))
  }

  test("fold right") {
    assert(List.foldRight(List(1,2,3), Nil:List[Int])((x, y) => Cons(x,y)) == List(1,2,3))
  }

  test("length") {
    assert(List.length(List(1,2,3)) == 3)
  }

  test("length2") {
    assert(List.length2(List(1,2,3)) == 3)
  }

  test("reverse") {
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
    assert(List.reverse(List(5,4,6,1,100)) == List(100,1,6,4,5))
  }

  test("append2") {
    assert(List.append2(List("foo", "blah", "baz"), List("test", "thing")) == List("foo", "blah", "baz", "test", "thing"))
  }

  test("concat list of list") {
    assert(List.concatListOfLists(List(List(1,3,5), List(2,4,6), List(2845,82945,6345))) == List(1,3,5,2,4,6,2845,82945,6345))
  }

  test("add1") {
    assert(List.add1(List(1,2,3)) == List(2,3,4))
  }

  test("double to string") {
    assert(List.doubleToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

  test("map") {
    assert(List.map(List(1, 2, 3))(x => x + 5) == List(6,7,8))
  }

  test("filter") {
    assert(List.filter(List(1,2,3,4,5,6,7,8,9,10))(x => x % 2 == 0) == List(2,4,6,8,10))
  }

  test("flatMap") {
    assert(List.flatMap(List(1,2,3))(i => List(i, i)) == List(1,1,2,2,3,3))
  }

  test("zip") {
    assert(List.zip(List(1,2,3), List(4,5,6))((x, y) => x + y) == List(5,7,9))
  }
}
