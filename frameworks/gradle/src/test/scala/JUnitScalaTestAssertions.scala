import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

class ScalaAdditionTest extends AssertionsForJUnit {
  @Test
  def testAdd() {
    assertEquals(2, 1 + 1)
  }
}
