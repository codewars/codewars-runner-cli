import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runners.JUnit4;
public class ExampleTest {
    @Test
    public void myTestFunction(){
        Example e = new Example();
        assertEquals("Failed Message", "foo", e.foo());
    }
}
