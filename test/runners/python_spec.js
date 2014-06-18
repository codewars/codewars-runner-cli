var expect = require('chai').expect;
var runner = require('../../lib/runners/python');


describe( 'python runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'python', solution: 'print 42'}, function(buffer) {
                expect(buffer.stdout ).to.equal('42\n');
                done();
            });
        });
    });
    describe( 'cw-2', function(){
        it( 'should handle a basic assertion', function(done){
            runner.run({language: 'python',
                        solution: 'a = 1',
                        fixture: 'Test.expect(a == 1)',
                        testFramework: 'cw-2'},
                       function(buffer){
                console.log(buffer);
                expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                done();
            });
        });
        it( 'should handle a failed assertion', function(done){
            runner.run({language: 'python',
                        solution: 'a = 1',
                        fixture: 'Test.expect(a == 2)',
                        testFramework: 'cw-2'},
                       function(buffer){
                console.log(buffer);
                expect(buffer.stdout).to.equal('<FAILED::>Value is not what was expected\n');
                done();
            });
        });
    });
    describe( 'unittest', function(){
        it( 'should handle a basic assertion', function(done){
            runner.run({language: 'python',
                        solution: 'a = 1',
                        fixture: 'class TestSequenceFunctions(unittest.TestCase):\n'
                               + '  def test_assert(self):\n'
                               + '    self.assertEqual(a, 1)\n'
                               + '_testsuite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)',
                        testFramework: 'unittest'},
                       function(buffer){
                console.log(buffer);
                expect(buffer.stdout).to.equal('<PASSED::>Test Passed\n');
                done();
            });
        });
        it( 'should handle a failed assetion', function(done){
            runner.run({language: 'python',
                        solution: 'a = 1',
                        fixture: 'class TestSequenceFunctions(unittest.TestCase):\n'
                               + '  def test_assert(self):\n'
                               + '    self.assertEqual(a, 2, "test failed")\n'
                               + '_testsuite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)',
                        testFramework: 'unittest'},
                       function(buffer){
                console.log(buffer);
                expect(buffer.stdout).to.equal('<FAILED::>test failed\n');
                done();
            });
        });
        it( 'should handle a failed assetion', function(done){
            runner.run({language: 'python',
                        solution: 'a = 1',
                        fixture: 'class TestSequenceFunctions(unittest.TestCase):\n'
                               + '  def test_assert(self):\n'
                               + '    raise StandardError("exception")\n'
                               + '_testsuite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)',
                        testFramework: 'unittest'},
                       function(buffer){
                console.log(buffer);
                expect(buffer.stdout).to.equal('<FAILED::>Unhandled Exception: exception\n');
                done();
            });
        });
    });

});
