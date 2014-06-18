import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;

public class CwRunListener extends RunListener
{
    public void testFailure(Failure failure)
    {
        System.out.println("wow failed");
    }
}
