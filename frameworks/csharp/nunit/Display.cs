using System;
using System.IO;

namespace NUnit.ConsoleRunner
{
	public static class Display
	{
		public static void Passed(string message)
		{
			Write("PASSED", message);
		}

		public static void Failed(string message)
		{
			Write("FAILED", message);
		}

		public static void Error(string message)
		{
			Write("ERROR", message);
		}

		public static void Log(string message, string label = "", string mode = "")
		{
			if (String.IsNullOrEmpty(message))
			{
				Write("LOG", message, label, mode);
			}
		}

		public static void Tab(string message, string label = "", string mode = "")
		{
			if (String.IsNullOrEmpty(message))
			{
				Write("TAB", message, label, mode);
			}
		}

		public static void Prop(string name, string value)
		{
			Write("PROP", value, label: name);
		}

		public static void Serialize(object obj, string label = "", string type = "LOG")
		{
			string message = Newtonsoft.Json.JsonConvert.SerializeObject(obj);
			Write(type, message, label: label, mode: "JSON");
		}

		public static void Write(string type, string message, string label = "", string mode = "")
		{
			string line = String.Format("\n<{0}:{1}:{2}>{3}", type.ToUpper(), mode, label, FormatMessage(message));
			Console.WriteLine(line);
		}

		public static string FormatMessage(string s)
		{
			if (s != null)
			{
				return s.Replace("\n", "<:LF:>");
			}
			else
			{
				return "";
			}
		}
	}
}

