package datastructures

import org.scalatest.FunSuite
import datastructures.Tree

class TreeTest extends FunSuite {
  val tree = Branch(Branch(Branch(Leaf(1), Leaf(7)), Leaf(2)), Branch(Leaf(3), Leaf(-10)))

  test("size") {
    assert(Tree.size(tree) == 9)
  }

  test("size fold") {
    assert(Tree.sizeFold(tree) == 9)
  }

  test("maximum") {
    assert(Tree.maximum(tree) == 7)
  }

  test("maximum fold") {
    assert(Tree.maximumFold(tree) == 7)
  }

  test("depth") {
    assert(Tree.depth(tree) == 4)
  }

  test("depth fold") {
    assert(Tree.depthFold(tree) == 4)
  }

  test("map") {
    assert(Tree.map(tree)(x => x + 1) == Branch(Branch(Branch(Leaf(2), Leaf(8)), Leaf(3)), Branch(Leaf(4), Leaf(-9))))
  }

  test("map fold") {
    assert(Tree.mapFold(tree)(x => x + 1) == Branch(Branch(Branch(Leaf(2), Leaf(8)), Leaf(3)), Branch(Leaf(4), Leaf(-9))))
  }
}
