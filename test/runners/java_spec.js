var expect = require('chai').expect;
var runner = require('../../lib/runners/java');


describe( 'java runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'java',
                        solution: 'class Solution {\n'
                                + '    public static void main(String[] args){\n'
                                + '        System.out.println("42");\n'
                                + '    }\n'
                                + '}\n'
                    }, function(buffer) {
                expect(buffer.stdout ).to.equal('42\n');
                done();
            });
        });
    });
    describe( 'junit', function(){
        it('should handle basic junit tests', function(done){
            runner.run({language: 'java',
                        solution: 'public class Solution {\n'
                                + '    public Solution(){}\n'
                                + '    public int testthing(){return 3;}\n'
                                + '    public static void main(String[] args){\n'
                                + '        System.out.println("42");\n'
                                + '    }\n'
                                + '}\n',
                        fixture: 'import static org.junit.Assert.assertEquals;\n'
                               + 'import org.junit.Test;\n'
                               + 'import org.junit.runners.JUnit4;\n'
                               + 'public class TestFixture {\n'
                               + '    public TestFixture(){}'
                               + '    @Test\n'
                               + '    public void myTestFunction(){\n'
                               + '        Solution s = new Solution();\n'
                               + '        assertEquals("wow", 3, s.testthing());\n'
                               + '        System.out.println("test out");\n'
                               + '}}'
                    }, function(buffer) {
                expect(buffer.stdout ).to.equal('<DESCRIBE::>myTestFunction(TestFixture)\ntest out\n<PASSED::>Test Passed\n');
                done();
            });
        });
        it('should handle junit tests failing', function(done){
            runner.run({language: 'java',
                        solution: 'public class Solution {\n'
                                + '    public Solution(){}\n'
                                + '    public int testthing(){return 3;}\n'
                                + '    public static void main(String[] args){\n'
                                + '        System.out.println("42");\n'
                                + '    }\n'
                                + '}\n',
                        fixture: 'import static org.junit.Assert.assertEquals;\n'
                               + 'import org.junit.Test;\n'
                               + 'import org.junit.runners.JUnit4;\n'
                               + 'public class TestFixture {\n'
                               + '    public TestFixture(){}'
                               + '    @Test\n'
                               + '    public void myTestFunction(){\n'
                               + '        Solution s = new Solution();\n'
                               + '        assertEquals("Failed Message", 5, s.testthing());\n'
                               + '        System.out.println("test out");\n'
                               + '}}'
                    }, function(buffer) {
                expect(buffer.stdout ).to.equal('<DESCRIBE::>myTestFunction(TestFixture)\n<FAILED::>Failed Message expected:<5> but was:<3>\n');
                done();
            });
        });
    });
});
