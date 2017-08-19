import io.kotlintest.matchers.shouldBe
import io.kotlintest.specs.StringSpec

class KotlintestTests : StringSpec() {
  init {
    "strings.length should return size of string" {
      "hello".length shouldBe 5
    }
  }
}
