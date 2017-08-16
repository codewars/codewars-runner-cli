import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._

object F {
  def add(a: Int, b: Int) = a + b
}

@RunWith(classOf[JUnitRunner])
class ScalaTestFunSuite extends FunSuite {
  test("add(1, 1) == 2") {
    assert(F.add(1, 1) == 2)
  }
}

@RunWith(classOf[JUnitRunner])
class ScalaTestFlatSpec extends FlatSpec {
  "add(1, 1)" should "return 2" in {
    assert(F.add(1, 1) == 2)
  }
}

@RunWith(classOf[JUnitRunner])
class ScalaTestFunSpec extends FunSpec {
  describe("add(1, 1)") {
    it("should return 2") {
      assert(F.add(1, 1) == 2)
    }
  }
}

