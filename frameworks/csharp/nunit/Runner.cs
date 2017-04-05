// ****************************************************************
// Copyright 2008, Charlie Poole
// This is free software licensed under the NUnit license. You may
// obtain a copy of the license at http://nunit.org
// ****************************************************************
using System;
using System.IO;
using System.Reflection;
using System.Drawing;
using NUnit.Core;
using NUnit.Util;


namespace NUnit.ConsoleRunner
{
	/// <summary>
	/// Summary description for Runner.
	/// </summary>
	public class Runner
	{
		static Logger log = InternalTrace.GetLogger(typeof(Runner));

		public static int Run(string[] args)
		{
			ConsoleOptions options = new ConsoleOptions(args);

            // Create SettingsService early so we know the trace level right at the start
            SettingsService settingsService = new SettingsService();
            InternalTraceLevel level = (InternalTraceLevel)settingsService.GetSetting("Options.InternalTraceLevel", InternalTraceLevel.Default);
            if (options.trace != InternalTraceLevel.Default)
                level = options.trace;

            InternalTrace.Initialize("nunit-console_%p.log", level);

            log.Info("NUnit-console.exe starting");

			// Add Standard Services to ServiceManager
			ServiceManager.Services.AddService( settingsService );
			ServiceManager.Services.AddService( new DomainManager() );
			//ServiceManager.Services.AddService( new RecentFilesService() );
			ServiceManager.Services.AddService( new ProjectService() );
			//ServiceManager.Services.AddService( new TestLoader() );
			ServiceManager.Services.AddService( new AddinRegistry() );
			ServiceManager.Services.AddService( new AddinManager() );
            ServiceManager.Services.AddService( new TestAgency() );

			// Initialize Services
			ServiceManager.Services.InitializeServices();

            foreach (string parm in options.Parameters)
            {
                if (!Services.ProjectService.CanLoadProject(parm) && !PathUtils.IsAssemblyFileType(parm))
                {
                    Console.WriteLine("File type not known: {0}", parm);
                    return ConsoleUi.INVALID_ARG;
                }
            }

			try
			{
				ConsoleUi consoleUi = new ConsoleUi();
				return consoleUi.Execute( options );
			}
			catch( FileNotFoundException ex )
			{
				Console.WriteLine( ex.Message );
				return ConsoleUi.FILE_NOT_FOUND;
			}
			catch( Exception ex )
			{
				Console.WriteLine( "Unhandled Exception:\n{0}", ex.ToString() );
				return ConsoleUi.UNEXPECTED_ERROR;
			}
			finally
			{
				if(options.wait)
				{
					Console.Out.WriteLine("\nHit <enter> key to continue");
					Console.ReadLine();
				}

				log.Info( "NUnit-console.exe terminating" );
			}

		}
	}
}
