package codewars.java;

import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;

public class CwRunListener extends RunListener
{
    private boolean failed;
    public void testFailure(Failure failure)
    {
        failed = true;
        System.out.println(String.format("<FAILED::>%s<:LF:>", formatMessage(failure.getMessage())));
    }
    public void testStarted(Description description)
    {
        System.out.println(String.format("<DESCRIBE::>%s<:LF:>", formatMessage(description.getDisplayName())));
        failed = false;
    }
    public void testFinished(Description description)
    {
        if(!failed)
        {
            System.out.println("<PASSED::>Test Passed<:LF:>");
        }
    }
    private static String formatMessage(String s)
    {
        return s.replaceAll("\n", "<:LF:>");
    }
}
