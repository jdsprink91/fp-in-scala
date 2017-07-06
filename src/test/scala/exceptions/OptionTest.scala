package exceptions

import org.scalatest.FunSuite
import exceptions.Option

class OptionTest extends FunSuite {
  test("both match") {
    assert(Option.bothMatch("^Foo.*", ".*Baz$", "Foo is totes the Baz") == Some(true))
  }

  test("sequence") {
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1,2,3)))
    assert(Option.sequence(List(Some(1), None, Some(3))) == None)
  }

}
