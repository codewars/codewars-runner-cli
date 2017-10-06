import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import org.junit.runner.Description;
import org.junit.runner.Result;
import java.io.StringWriter;
import java.io.PrintWriter;

public class CwRunListener extends RunListener
{
    private boolean failed;

    public void testFailure(final Failure failure)
    {
        failed = true;
        final String msg = failure.getMessage();
        final boolean hasMessage =  msg != null && msg.length() > 0;
        System.out.println(String.format("\n<FAILED::>%s", formatMessage(hasMessage ? msg : "Runtime Error Occurred")));
        if(failure.getException() != null) {
            String prefix = "";
            if (hasMessage) {
              prefix = "<LOG::-Exception Details>";
            }

            System.out.println(prefix + formatMessage(formatException(failure.getException())));
        }
    }
    public void testStarted(final Description description)
    {
        System.out.println(String.format("\n<IT::>%s", formatMessage(description.getDisplayName())));
        failed = false;
    }
    public void testFinished(final Description description)
    {
        if(!failed)
        {
            System.out.println("\n<PASSED::>Test Passed");
        }
        System.out.println("\n<COMPLETEDIN::>");
    }

    public void testRunStarted(final Description description) {
        final String name = description.getDisplayName();
        final boolean hasName = name != null && name.length() > 0 && name != "null";
        System.out.println(String.format("\n<DESCRIBE::>%s", formatMessage(hasName ? name : "Test Suite")));
    }

    public void testRunFinished(final Result result) {
        System.out.println(String.format("\n<COMPLETEDIN::>%sms", result.getRunTime()));
    }

    private static String formatException(final Throwable ex)
    {
        if(ex == null){
            return "";
        }
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
        return sw.toString().replaceAll("\tat (?:org.junit.(?:internal|runners?)|sun.reflect|org.gradle|java|com.sun|Start.start).*\n", "");
    }
    private static String formatMessage(final String s)
    {
        if(s == null){
            return "";
        }
        return s.replaceAll("\n", "<:LF:>");
    }
}
