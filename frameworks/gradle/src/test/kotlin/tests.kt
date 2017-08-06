import kotlin.test.assertEquals
import org.junit.Test

class KotlinHelloWorldTest {
    @Test
    fun greetingTest() {
        assertEquals("Hello World! from Kotlin", getGreeting())
    }
}
