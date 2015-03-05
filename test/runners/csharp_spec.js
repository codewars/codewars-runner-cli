var expect = require('chai').expect;
var runner = require('../../lib/runners/csharp');

describe( 'c# runner', function(){
    describe( '.run', function()
    {
        it('should handle basic code evaluation', function (done)
        {
            runner.run({language: 'csharp', solution: 'Console.WriteLine(1+1);'}, function (buffer)
            {
                expect(buffer.stdout).to.equal('2\n');
                done();
            });
        });

        it('should handle basic code evaluation from file', function (done)
        {
            runner.run({language: 'csharp', solutionFile: 'test/csharp/solution1.cs'}, function (buffer)
            {
                expect(buffer.stdout).to.equal('Hello World\n');
                done();
            });
        });

//        it('should handle fork bomb', function (done)
//        {
//            runner.run({language: 'csharp', solutionFile: 'test/csharp/solution2.cs'}, function (buffer)
//            {
//                expect(buffer.stdout).to.contain('System.SystemException: Thread creation failed');
//                done();
//            });
//        });
//
//        it('should block network connections', function (done)
//        {
//            runner.run({language: 'csharp', solutionFile: 'test/csharp/solution3.cs'}, function (buffer)
//            {
//                expect(buffer.stdout).to.contain('System.Net.WebException:');
//                done();
//            });
//        });
    });
});