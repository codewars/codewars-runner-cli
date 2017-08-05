package codewars

import kotlin.test.assertEquals
import org.junit.Test

class TestHelloWorld {
    @Test
    fun greetingTest() {
        assertEquals("Hello, world!", getGreeting())
    }
}
