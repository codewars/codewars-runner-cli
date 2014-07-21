package codewars.java;

import org.junit.runner.JUnitCore;

public class CwTestRunner
{
    public static void main(String[] args)
    {
        JUnitCore core = new JUnitCore();
        core.addListener(new CwRunListener());
        //core.run(TestFixture.class);
    }
}
