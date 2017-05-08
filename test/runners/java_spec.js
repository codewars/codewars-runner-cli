var expect = require('chai').expect;
var runner = require('../runner');


describe('java runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'java',
        code:`
          class Solution {
             public static void main(String[] args){
                  System.out.println("42");
             }
          }`
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('42\n');
        done();
      });
    });
  });
  describe('junit', function() {
    it('should handle basic junit tests', function(done) {
      runner.run({
        language: 'java',
        code: `public class Solution {
                public Solution(){}
                public int testthing(){return 3;}
              }`,
        fixture: `import static org.junit.Assert.assertEquals;
                    import org.junit.Test;
                    import org.junit.runners.JUnit4;
                    public class TestFixture {
                        public TestFixture(){}
                        @Test
                        public void myTestFunction(){
                            Solution s = new Solution();
                            assertEquals("wow", 3, s.testthing());
                            System.out.println("test out");
                    }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>myTestFunction(TestFixture)<:LF:>\ntest out\n\n<PASSED::>Test Passed<:LF:>\n');
        done();
      });
    });
    it('should handle junit tests failing', function(done) {
      runner.run({
        language: 'java',
        code: `public class Solution {
                        public Solution(){}
                        public int testthing(){return 3;}
                    }`,
        fixture: `import static org.junit.Assert.assertEquals;
                    import org.junit.Test;
                    import org.junit.runners.JUnit4;
                    public class TestFixture {
                        public TestFixture(){}
                        @Test
                        public void myTestFunction(){
                            Solution s = new Solution();
                            assertEquals("Failed Message", 5, s.testthing());
                            System.out.println("test out");
                    }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>myTestFunction(TestFixture)<:LF:>\n\n<FAILED::>Failed Message expected:<5> but was:<3><:LF:>\n');
        done();
      });
    });
    it('should report junit messages', function(done) {
      runner.run({
        language: 'java',
        code: `public class Solution {
                        public Solution(){}
                        public String testthing(){ return null; }
                    }`,
        fixture: `import static org.junit.Assert.assertEquals;
                    import org.junit.Test;
                    import org.junit.runners.JUnit4;
                    public class TestFixture {
                        @Test
                        public void myTestFunction(){
                            Solution s = new Solution();
                            assertEquals("Failed Message", 1, s.testthing().length());
                    }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>Runtime Error Occurred');
        expect(buffer.stdout).to.contain('NullPointerException');
        done();
      });
    });
  });
});
