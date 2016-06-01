package codewars.java;

import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;

public class CwRunListener extends RunListener
{
    private boolean failed;
    public void testFailure(final Failure failure)
    {
        failed = true;
        final String msg = failure.getMessage();
        System.out.println(String.format("<FAILED::>%s<:LF:>", formatMessage(msg != null && msg.length() > 0 ? msg : "Unknown Test Failure")));
    }
    public void testStarted(final Description description)
    {
        System.out.println(String.format("<DESCRIBE::>%s<:LF:>", formatMessage(description.getDisplayName())));
        failed = false;
    }
    public void testFinished(final Description description)
    {
        if(!failed)
        {
            System.out.println("<PASSED::>Test Passed<:LF:>");
        }
        System.out.println("<COMPLETEDIN::>");
    }
    private static String formatMessage(final String s)
    {
        if(s == null){
            return "";
        }
        return s.replaceAll("\n", "<:LF:>");
    }
}
