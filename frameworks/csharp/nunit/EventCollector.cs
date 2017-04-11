// ****************************************************************
// Copyright 2007, Charlie Poole
// This is free software licensed under the NUnit license. You may
// obtain a copy of the license at http://nunit.org
// ****************************************************************
using System;
using System.IO;
using System.Diagnostics;
using System.Text.RegularExpressions;
using System.Collections;
using System.Collections.Specialized;
using NUnit.Core;
using NUnit.Util;

namespace NUnit.ConsoleRunner
{

	/// <summary>
	/// Summary description for EventCollector.
	/// </summary>
	public class EventCollector : MarshalByRefObject, EventListener
	{
		private int testRunCount;
		private int failureCount;
		private int level;

		private ConsoleOptions options;
		private TextWriter outWriter;
		private TextWriter errorWriter;
		private StringWriter outputWriter;

		StringCollection messages;

		private bool progress = false;
		private string currentTestName;

		private ArrayList unhandledExceptions = new ArrayList();
		private ArrayList describes = new ArrayList();

		public EventCollector( ConsoleOptions options, TextWriter outWriter, TextWriter errorWriter )
		{
			level = 0;
			this.options = options;
			this.outWriter = outWriter;
			this.errorWriter = errorWriter;
			this.currentTestName = string.Empty;
			this.progress = !options.xmlConsole && !options.labels && !options.nodots;

			AppDomain.CurrentDomain.UnhandledException +=
				new UnhandledExceptionEventHandler(OnUnhandledException);
		}

		public bool HasExceptions
		{
			get { return unhandledExceptions.Count > 0; }
		}

		public void WriteExceptions()
		{
			Console.WriteLine();
			Console.WriteLine("Unhandled exceptions:");
			int index = 1;
			foreach( string msg in unhandledExceptions )
				Console.WriteLine( "{0}) {1}", index++, msg );
		}

		public void RunStarted(string name, int testCount)
		{
		}

		public void RunFinished(TestResult result)
		{
		}

		public void RunFinished(Exception exception)
		{

		}

		public void TestStarted(TestName testName)
		{
			this.outputWriter = new StringWriter();
		}

		public void TestFinished(TestResult testResult)
		{
			if (testResult.Test.ClassName != null && !describes.Contains(testResult.Test.ClassName)) {
				Display.Write("describe", testResult.Test.ClassName);
				describes.Add (testResult.Test.ClassName);
			}
			if (String.IsNullOrEmpty(testResult.Description))
			{
				if (testResult.Test.MethodName != null)
				{
					Display.Write("IT", testResult.Test.MethodName);
				}
			}
			else
			{
				Display.Write("IT", testResult.Description);
			}

			foreach(string category in testResult.Test.Categories)
			{
				Display.Prop("tag", category);
			}

			foreach (string key in testResult.Test.Properties.Keys)
			{
				if (!key.StartsWith("_", StringComparison.InvariantCulture))
				{
					Display.Prop(key, testResult.Test.Properties[key].ToString());
				}
			}

			string stackTrace = testResult.StackTrace != null ? StackTraceFilter.Filter (testResult.StackTrace).Trim() : null;
			if (String.IsNullOrEmpty(stackTrace))
			{
				stackTrace = testResult.StackTrace;
			}

			Console.Write(outputWriter.ToString());

			if (testResult.IsSuccess)
			{
				Display.Passed(testResult.Message == null ? "Test Passed" : testResult.Message);
				testRunCount++;
			}
			else if (testResult.IsFailure)
			{
				testRunCount++;
				failureCount++;
				if (testResult.Results != null) Console.WriteLine(testResult.Results.Count);
				if (testResult.AssertCount > 1)
				{
					Display.Log(String.Format("{0} passed", AssertionsName(testResult.AssertCount - 1)));
				}

				Display.Failed(testResult.Message);
				//Display.Serialize(testResult, label: "-Test Result Info");
				Display.Log(testResult.StackTrace, label: "-Stack Trace");
			}
			else if (testResult.IsError)
			{
				Display.Error(testResult.Message);
				//Display.Serialize(testResult, label: "-Test Result Info");
				Display.Log(stackTrace, mode: "ESC", label: "Stack Trace");
			}

			Console.WriteLine(String.Format("<COMPLETEDIN::>{0}", testResult.Time));

			currentTestName = string.Empty;
		}

		public void SuiteStarted(TestName testName)
		{
			if ( level++ == 0 )
			{
				//messages = new StringCollection();
				//testRunCount = 0;
				//testIgnoreCount = 0;
				//failureCount = 0;
				//Trace.WriteLine( "################################ UNIT TESTS ################################" );
				//Trace.WriteLine( "Running tests in '" + testName.FullName + "'..." );
//				Console.WriteLine ("<DESCRIBE::>" + FormatMessage (testName.UniqueName));
			}
		}

		public void SuiteFinished(TestResult suiteResult)
		{
//			Console.WriteLine (unhandledExceptions.Count.ToString());
//			Console.WriteLine (suiteResult.IsFailure.ToString ());
//			Console.WriteLine (suiteResult.StackTrace);
		}

		private void OnUnhandledException(object sender, UnhandledExceptionEventArgs e)
		{
			if (e.ExceptionObject.GetType() != typeof(System.Threading.ThreadAbortException))
			{
				this.UnhandledException((Exception)e.ExceptionObject);
			}
		}

		private string AssertionsName(int count)
		{
			if (count == 1)
			{
				return "1 assertion";
			}
			else
			{
				return String.Format("{0} assertions", count);
			}
		}


		public void UnhandledException( Exception exception )
		{
			// If we do labels, we already have a newline
			unhandledExceptions.Add(currentTestName + " : " + exception.ToString());
			//if (!options.labels) outWriter.WriteLine();
			string msg = string.Format("##### Unhandled Exception while running {0}", currentTestName);
			//outWriter.WriteLine(msg);
			//outWriter.WriteLine(exception.ToString());

			Display.Error(msg + "\n" + exception.ToString());
			Trace.WriteLine(exception.ToString());
		}

		public void TestOutput( TestOutput output)
		{
			switch ( output.Type )
			{
				case TestOutputType.Out:
					outputWriter.Write( output.Text );
					break;
				case TestOutputType.Error:
					errorWriter.Write( output.Text );
					break;
			}
		}


		public override object InitializeLifetimeService()
		{
			return null;
		}
	}
}
