var expect = require('chai').expect;
var runner = require('../runner');


describe('java runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'java',
        code: [
          'class Solution {',
          '   static void main(String[] args){',
          '        System.out.println("42");',
          '   }',
          '}'].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.contain('42\n');
        done();
      });
    });
  });
  describe('junit', function() {
    it('should handle basic junit tests', function(done) {
      runner.run({
        language: 'java',
        code: 'public class Solution {\n'
                    + '    public Solution(){}\n'
                    + '    public int testthing(){return 3;}\n'
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
        expect(buffer.stdout).to.contain('<DESCRIBE::>myTestFunction(TestFixture)<:LF:>\ntest out\n\n<PASSED::>Test Passed<:LF:>\n');
        done();
      });
    });
    it('should handle junit tests failing', function(done) {
      runner.run({
        language: 'java',
        code: 'public class Solution {\n'
                    + '    public Solution(){}\n'
                    + '    public int testthing(){return 3;}\n'
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
        expect(buffer.stdout).to.contain('<DESCRIBE::>myTestFunction(TestFixture)<:LF:>\n\n<FAILED::>Failed Message expected:<5> but was:<3><:LF:>\n');
        done();
      });
    });
    it('should report junit messages', function(done) {
      runner.run({
        language: 'java',
        code: 'public class Solution {\n'
                    + '    public Solution(){}\n'
                    + '    public String testthing(){ return null; }\n'
                    + '}\n',
        fixture: 'import static org.junit.Assert.assertEquals;\n'
                    + 'import org.junit.Test;\n'
                    + 'import org.junit.runners.JUnit4;\n'
                    + 'public class TestFixture {\n'
                    + '    @Test\n'
                    + '    public void myTestFunction(){\n'
                    + '        Solution s = new Solution();\n'
                    + '        assertEquals("Failed Message", 1, s.testthing().length());\n'
                    + '}}'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>Runtime Error Occurred');
        expect(buffer.stdout).to.contain('NullPointerException');
        done();
      });
    });
  });
});
