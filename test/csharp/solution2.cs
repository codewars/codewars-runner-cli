using System.Threading;

public class Program{
	public static void Main(string[] args){

			System.Diagnostics.ProcessStartInfo si = new System.Diagnostics.ProcessStartInfo();
			si.CreateNoWindow = true;
			si.RedirectStandardOutput = true;
			si.UseShellExecute = false;
			si.FileName = "bash";
			si.Arguments = "-c \"echo `ulimit -u`\"";
			Console.WriteLine(System.Diagnostics.Process.Start(si).StandardOutput.ReadToEnd());
		while (true)   {
				Thread thread = new Thread(new ThreadStart(Program.Method));
				thread.Start();
		}
	}
	static void Method(){
			Thread.Sleep(500);
			Console.WriteLine("Hello World");
	}
}
