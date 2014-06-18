import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;

public class CwRunListener extends RunListener
{
    private boolean failed;
    private boolean anyfails;
    public void testFailure(Failure failure)
    {
        failed = true;
        anyfails = true;
        System.out.println("<FAILED::>" + failure.getMessage());
    }
    public void testStarted(Description description)
    {
        failed = false;
    }
    public void testFinished(Description description)
    {
        if(!failed)
        {
            System.out.println("<PASSED::>Test Passed");
        }
    }
    public void testRunStarted(Description description)
    {
        anyfails = false;
    }
    public void testRunFinished(Description description)
    {
        if(!anyfails)
        {
            System.out.println("<PASSED::>All Tests Passed");
        }
    }
}
