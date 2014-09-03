using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using ServiceStack;

public class Hello1
{
   public static void Main()
   {
      throw new Exception("asdfdf\naaaa");
   }
}

// ReSharper disable once CheckNamespace
// ReSharper disable once UnusedMember.Global
public static class Test
{
    private static bool describing;
    // ReSharper disable NotAccessedField.Local
    private static int correct;
    private static int incorrect;
    // ReSharper restore NotAccessedField.Local

    private static readonly List<string> html = new List<string>();
    private static readonly List<Error> failed = new List<Error>();

    private static readonly List<Action> beforeCallbacks = new List<Action>();
    private static readonly List<Action> afterCallbacks = new List<Action>();

    public static bool AllowHtmlOutput = false;

    public class Error : Exception
    {
        //No need in c#. public string Name { get { return "Test:Error"; } }

        internal Error(string message)
            : base(message ?? "")
        {
            LogCall("Error");
        }
    }

    #region Utility Functions

    private static readonly Dictionary<string, int> methodCalls = new Dictionary<string, int>(); // left case sensitive

    private static int GetCallCount(string name)
    {
        int count;
        if (methodCalls.TryGetValue(name, out count))
            return count;

        return 0;
    }

    private static void SetCallCount(string name, int count)
    {
        methodCalls[name] = count;
    }

    private static void LogCall(string name, bool useConsole = false)
    {
        SetCallCount(name, GetCallCount(name) + 1);

        if (useConsole)
        {
            Console.WriteLine("{0} called", name);
        }
    }

    internal static string Message(object msg = null, string prefix = null)
    {
        if (null == msg) return null;

        var str = msg as string;
        if (str != null)
            return Message(str, prefix);

        var ienum = msg as IEnumerable;
        if (null != ienum)
            return Message(ienum.Cast<object>(), prefix);

        var fn = msg as Func<string>;
        if (null != fn)
            return Message(fn(), prefix);

        var fno = msg as Func<object>;
        if (null != fno)
            return Message(fno, prefix);

        return Message(Convert.ToString(msg), prefix);
    }

    internal static string Message(IEnumerable<object> msg = null, string prefix = null)
    {
        if (null != msg)
            return Message(CombineMessages(msg, " - "), prefix);

        return Message(string.Empty, prefix);
    }

    internal static string Message(Func<object> msg = null, string prefix = null)
    {
        if (null != msg)
            return Message(Convert.ToString(msg()), prefix);

        return Message(string.Empty, prefix);
    }

    internal static string Message(string msg = null, string prefix = null)
    {
        return !string.IsNullOrWhiteSpace(prefix)
            ? string.Format("{0} - {1}", prefix, msg)
            : msg;
    }

    private static string CombineMessages(IEnumerable<object> msg, string separator)
    {
        if (null == msg)
            return string.Empty;
        return string.Join(separator, msg.Where(m => null != m).Select(Convert.ToString));
    }

    private static string CombineMessages(string separator, params object[] msg)
    {
        if (null == msg)
            return string.Empty;
        return string.Join(separator, msg.Where(m => null != m).Select(Convert.ToString));
    }

    private static Regex tagFinder = new Regex("<[^>].*>");

    private static string LogFilter(string msg, bool lf = false)
    {
        if (AllowHtmlOutput)
            return msg;

        var m = tagFinder.Replace((msg ?? ""), "");
        // Add a LF only for empty messages that don't have the *lf* set
        return (m.Length == 0 && (!lf)) ? "" : m + "\n";
    }

    private static void Write(string msg = null, bool noLineBreak = false)
    {
        if (string.IsNullOrWhiteSpace(msg))
            return;

        if (0 == html.Count)
            Console.WriteLine(msg);
        else
        {
            html.Add(msg);
            if (!noLineBreak)
                html.Add(LogFilter("<br>"));
        }
    }

    #endregion

    public static void Expect(bool passed, object msg = null, ExpectOptions options = null)
    {
        LogCall("Expect");
        options = options ?? new ExpectOptions();

        if (passed)
        {
            var successMsg = "Test Passed";
            if (!string.IsNullOrWhiteSpace(options.SuccessMsg))
                successMsg += ": " + options.SuccessMsg;

            Write(LogFilter("<div class=\"console-passed\">") + successMsg +
                  LogFilter("</div>", true), true);
            correct++;
        }
        else
        {
            msg = Message(msg) ?? "Invalid";

            // extra credit feature - TODO: complete
            if (null != options.ExtraCredit)
            {
                // Why msg is overriden here?
                msg = (!Equals(options.ExtraCredit, true)) ? Message(options.ExtraCredit) : null;
                msg = CombineMessages(": ", "Test Missed", msg);
                Write(LogFilter("<div class='console-missed'>") + msg +
                      LogFilter("</div>", true), true);
                incorrect++;
            }
            else
            {
                Write(LogFilter("<div class='console-failed'>Test Failed: ") + msg +
                      LogFilter("</div>", true), true);
                var error = new Error((string)msg); // at this point it should be string
                if (describing)
                    failed.Add(error);
                else
                    throw error;
            }

        }

    }

    public static int CallCount(string name)
    {
        return GetCallCount(name);
    }

    public static string Inspect(object obj)
    {
        return obj.ToJson();
    }

    private class ConsoleRedirector: TextWriter
    {
        private static readonly char[] newLineChars = Environment.NewLine.ToCharArray();

        public override Encoding Encoding
        {
            get { return Encoding.UTF8; }
        }

        private List<char> chars = new List<char>(128);

        public override void Write(char value)
        {
            chars.Add(value);
            if (chars.Count >= newLineChars.Length)
            {
                for (int i = 0, start = chars.Count - newLineChars.Length; i < newLineChars.Length; i++)
                    if (chars[start + i] != newLineChars[i])
                        return;

                Test.Write(string.Join("", chars));
                chars.Clear();
            }
        }
    }

    public static void Describe(object msg, Action fn = null)
    {
        var consoleOut = Console.Out;

        try
        {
            if (describing)
                throw new InvalidOperationException("cannot call describe within another describe");

            LogCall("Describe");
            describing = true;
            html.Add(LogFilter("<div class=\"console-describe\"><h6>"));
            html.Add(Message(msg));
            html.Add(LogFilter(":</h6>"));
            // intercept console.log messages
            Console.SetOut(new ConsoleRedirector());
            if (null != fn)
                fn();
        }
        finally
        {
            html.Add(LogFilter("</div>"));
            // restore log
            Console.SetOut(consoleOut);
            Console.WriteLine(string.Join("", html));
            html.Clear();
            describing = false;
            beforeCallbacks.Clear();
            afterCallbacks.Clear();
            if (failed.Count > 0)
                throw failed[0];
        }
    }

    public static void It(object msg, Action fn = null)
    {
        try
        {
            LogCall("It");
            html.Add(LogFilter("<div class=\"console-it\"><h6>"));
            html.Add(Message(msg));
            html.Add(LogFilter(":</h6>"));
            beforeCallbacks.ForEach(cb => cb());
            try
            {
                if (null != fn)
                    fn();
            }
            finally
            {
                afterCallbacks.ForEach(cb => cb());
            }
        }
        finally
        {
            html.Add(LogFilter("</div>"));
        }
    }

    public static void Before(Action cb)
    {
        beforeCallbacks.Add(cb);
    }

    public static void After(Action cb)
    {
        afterCallbacks.Add(cb);
    }

    public static void AssertEquals(object actual, object expected, object msg = null, ExpectOptions options = null)
    {
        LogCall("AssertEquals");
        if (!Equals(actual, expected))
        {
            msg = Message(string.Format("Expected: {0}, instead got: {1}", Inspect(expected), Inspect(actual)), Convert.ToString(msg));
            Expect(false, msg, options);
        }
        else
        {
            options = options ?? new ExpectOptions();
            options.SuccessMsg = options.SuccessMsg ?? string.Format("Value == {0}", Inspect(expected));
            Expect(true, null, options);
        }
    }

    public static void AssertNotEquals(object actual, object expected, object msg = null, ExpectOptions options = null)
    {
        LogCall("AssertNotEquals");
        if (Equals(actual, expected))
        {
            msg = Message("Not Expected: " + Inspect(actual), Convert.ToString(msg));
            Expect(false, msg, options);
        }
        else
        {
            options = options ?? new ExpectOptions();
            options.SuccessMsg = options.SuccessMsg ?? string.Format("Value != {0}", Inspect(expected));
            Expect(true, null, options);
        }
    }

    public static void AssertSimilar(object actual, object expected, object msg = null, ExpectOptions options = null)
    {
        LogCall("AssertSimilar");
        AssertEquals(Inspect(actual), Inspect(expected), msg, options);
    }

    public static void ExpectNoError(Action fn = null)
    {
        ExpectNoError("Unexpected error was raised", fn);
    }

    public static void ExpectNoError(string msg = null, Action fn = null)
    {
        LogCall("ExpectNoError");

        try
        {
            fn();
            Expect(true);
        }
        catch (Error)
        {
            throw;
        }
        catch (Exception ex)
        {
            msg += ": " + ex;
            Expect(false, msg);
        }
    }

    public static void ExpectError(Action fn = null, ExpectOptions options = null)
    {
        ExpectError("Unexpected error was raised.", fn, options);
    }

    public static void ExpectError(string msg = null, Action fn = null, ExpectOptions options = null)
    {
        LogCall("ExpectError");

        var passed = false;
        try
        {
            fn();
        }
        catch (Exception ex)
        {
            Console.WriteLine(LogFilter("<b>Expected error was thrown:</b> ") + ex);
            passed = true;
        }

        Expect(passed, msg, options);
    }

    public static double RandomNumber()
    {
        LogCall("RandomNumber");
        return Math.Round((new Random()).NextDouble() * 100);
    }

    private const string base36 = "0123456789abcdefghijklmnopqrstuvwxyz";
    private static int randomSeed = (int)(DateTime.Now.Ticks & 0xFFFFFFFF);

    public static string RandomToken()
    {
        var r = new Random(randomSeed);
        var token = string.Join("",
            Enumerable.Range(0, 8).Select(i => base36[r.Next(35)])
            );
        randomSeed = r.Next();

        return token;


    }

    public static Array Randomize(Array array)
    {
        LogCall("Randomize");
        var arr = array;
        var i = arr.Length;

        var r = new Random(randomSeed);

        while (0 != i)
        {
            var j = r.Next(i);
            randomSeed = r.Next();
            var x = arr.GetValue(--i);
            arr.SetValue(arr.GetValue(j), i);
            arr.SetValue(x, j);
        }
        return arr;
    }

    public static object Sample(Array array)
    {
        LogCall("Sample");
        var r = new Random(randomSeed);
        randomSeed = r.Next();
        return array.GetValue(r.Next(array.Length));
    }
}

public class ExpectOptions
{
    public string SuccessMsg { get; set; }
    public object ExtraCredit { get; set; }
}