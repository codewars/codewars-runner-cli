mv -f ../../NUnit-codewars/bin/Debug/nunit.framework.dll frameworks/csharp/nunit/nunit.framework.dll
mv -f ../../NUnit-codewars/bin/Debug/nunit.util.dll frameworks/csharp/nunit/nunit.util.dll
mv -f ../../NUnit-codewars/bin/Debug/nunit.core.dll frameworks/csharp/nunit/nunit.core.dll
mv -f ../../NUnit-codewars/bin/Debug/nunit.core.interfaces.dll frameworks/csharp/nunit/nunit.core.interfaces.dll
mv -f ../../NUnit-codewars/bin/Debug/nunit-console.exe frameworks/csharp/nunit/nunit-console.exe
mv -f ../../NUnit-codewars/bin/Debug/nunit-console.exe.config frameworks/csharp/nunit/nunit-console.exe.config
mv -f ../../NUnit-codewars/bin/Debug/log4net.dll frameworks/csharp/nunit/log4net.dll
cp -r ../../NUnit-codewars/bin/Debug/framework/* frameworks/csharp/nunit/framework
cp -r ../../NUnit-codewars/bin/Debug/lib/* frameworks/csharp/nunit/lib

mocha test/runners/csharp_spec
