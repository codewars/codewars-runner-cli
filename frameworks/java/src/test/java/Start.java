import org.junit.runner.JUnitCore;
import org.junit.Test;
import org.junit.runners.JUnit4;

public class Start {
    @Test
    public void start(){
        JUnitCore runner = new JUnitCore();
        runner.addListener(new CwRunListener());
        runner.run(ExampleTest.class);
    }
}


