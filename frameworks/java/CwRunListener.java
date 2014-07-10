import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;

public class CwRunListener extends RunListener
{
    private boolean failed;
    public void testFailure(Failure failure)
    {
        failed = true;
        System.out.println("<FAILED::>" + formatMessage(failure.getMessage()));
    }
    public void testStarted(Description description)
    {
        System.out.println("<DESCRIBE::>" + formatMessage(description.getDisplayName()));
        failed = false;
    }
    public void testFinished(Description description)
    {
        if(!failed)
        {
            System.out.println("<PASSED::>Test Passed");
        }
    }
    private static String formatMessage(String s)
    {
        return s.replaceAll("\n", "<:LF:>");
    }
}
