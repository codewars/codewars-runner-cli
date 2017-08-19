import org.junit.Test
import com.winterbe.expekt.expect
import com.winterbe.expekt.should

class ExpektTests {
  @Test
  fun canUseExpect() {
    expect(1).to.equal(1)
  }
  @Test
  fun canUseShould() {
    1.should.equal(1)
  }
}
