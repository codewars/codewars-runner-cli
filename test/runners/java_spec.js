var expect = require('chai').expect;
var runner = require('../runner');
var execSync = require('child_process').execSync;


describe('java runner', function() {

  console.log("Starting daemon with test run to ensure tests run within their allowed time...");
  console.log(execSync('sh /runner/frameworks/java/prewarm.sh').toString());

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

    it('should handle basic code evaluation on a custom class', function(done) {
      runner.run({
        language: 'java',
        code:`
          class Challenge {
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
        code: `
          public class Solution {
            public Solution(){}
            public int testthing(){return 3;}
          }`,
        fixture: `
          import static org.junit.Assert.assertEquals;
          import org.junit.Test;
          import org.junit.runners.JUnit4;
          public class TestFixture {
              public TestFixture(){}
              @Test
              public void myTestFunction(){
                  Solution s = new Solution();
                  assertEquals("wow", 3, s.testthing());
                  System.out.println("test out");
                  System.err.println("error test");
          }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<IT::>myTestFunction(TestFixture)\ntest out\n<PASSED::>Test Passed\n');
        done();
      });
    });

    it('should handle junit tests failing', function(done) {
      runner.run({
        language: 'java',
        code: `
          public class Solution {
              public Solution(){}
              public int testthing(){return 3;}
          }`,
        fixture: `
          import static org.junit.Assert.assertEquals;
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
        expect(buffer.stdout).to.contain('<IT::>myTestFunction(TestFixture)\n<FAILED::>Failed Message expected:<5> but was:<3>\n');
        expect(buffer.stdout).to.not.contain('at java.lang.reflect.Method');
        done();
      });
    });

    it('should report junit messages', function(done) {
      runner.run({
        language: 'java',
        code: `
          public class Solution {
              public Solution(){}
              public String testthing(){ return null; }
          }`,
        fixture: `
          import static org.junit.Assert.assertEquals;
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

    it('should handle custom class names', function(done) {
      runner.run({
        language: 'java',
        code: `
          public class Challenge {
            public Challenge(){}
            public int testthing(){return 3;}
          }`,
        fixture: `
          import static org.junit.Assert.assertEquals;
          import org.junit.Test;
          import org.junit.runners.JUnit4;
          public class Fixture {
              public Fixture(){}
              @Test
              public void myTestFunction(){
                  Challenge s = new Challenge();
                  assertEquals("wow", 3, s.testthing());
                  System.out.println("test out");
          }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<IT::>myTestFunction(Fixture)\ntest out\n<PASSED::>Test Passed\n');
        done();
      });
    });

    it('should handle packages', function(done) {
      runner.run({
        language: 'java',
        code: `
          package stuff;
          public class Challenge {
            public Challenge(){}
            public int testthing(){return 3;}
          }`,
        fixture: `
          import static org.junit.Assert.assertEquals;
          import org.junit.Test;
          import org.junit.runners.JUnit4;
          import stuff.Challenge;

          public class Fixture {
              public Fixture(){}
              @Test
              public void myTestFunction(){
                  Challenge s = new Challenge();
                  assertEquals("wow", 3, s.testthing());
                  System.out.println("test out");
          }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<IT::>myTestFunction(Fixture)\ntest out\n<PASSED::>Test Passed\n');
        done();
      });
    });

    it('should handle support setup code', function(done) {
      runner.run({
        language: 'java',
        code: `
          public class Challenge {
            public Challenge(){}
            public int testthing(){return 3;}
          }`,
        setup: `
          class Node<T> {
          }

          class Helpers {
            static String out() {
              return "test out";
            }
          }
        `,
        fixture: `
          import static org.junit.Assert.assertEquals;
          import org.junit.Test;
          import org.junit.runners.JUnit4;

          public class Fixture {
              public Fixture(){}
              @Test
              public void myTestFunction(){
                  Challenge s = new Challenge();
                  assertEquals("wow", 3, s.testthing());
                  System.out.println(Helpers.out());
          }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<IT::>myTestFunction(Fixture)\ntest out\n<PASSED::>Test Passed\n');
        done();
      });
    });

    it('should handle support split files', function(done) {
      runner.run({
        language: 'java',
        code: `
          public class Challenge {
            public Challenge(){}
            public int testthing(){return 3;}
          }

          // @config: split-file Helpers.java
          public class Helpers {
            static String out() {
              return "test out";
            }
          }
        `,
        fixture: `
          import static org.junit.Assert.assertEquals;
          import org.junit.Test;
          import org.junit.runners.JUnit4;

          public class Fixture {
              public Fixture(){}
              @Test
              public void myTestFunction(){
                  Challenge s = new Challenge();
                  assertEquals("wow", 3, s.testthing());
                  System.out.println(Helpers.out());
          }}`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<IT::>myTestFunction(Fixture)\ntest out\n<PASSED::>Test Passed\n');
        done();
      });
    });
  });

  describe('compiler warnings as STDERR messages', function() {
    it('should handle unchecked or unsafe operations', function(done) {
      runner.run({
        language: 'java',
        code: `
          import java.util.ArrayList;

          public class Example {
          
              public static String[] dirReduc(String[] arr) {
                  ArrayList<String> result = new ArrayList();
                  return result.toArray(new String[result.size()]);
              }
          }
        `,
        fixture: `
          import org.junit.Test;
          import static org.junit.Assert.assertEquals;
          
          public class ExampleTest {
            @Test
            public void testRandomDirReduc() throws Exception {
              assertEquals("I always pass!", 1, 1);
            }
          }`
      }, function(buffer) {
        console.log(`|${buffer.stderr}|`);
        expect(buffer.stderr).to.equal('');
        done();
      });

    });
  });

  describe('spring', function() {
    it('should handle basic junit tests', function(done) {
      runner.run({
        language: 'java',
        code: `
          package hello;

          import org.springframework.boot.autoconfigure.*;
          import org.springframework.stereotype.Controller;
          import org.springframework.web.bind.annotation.RequestMapping;
          import org.springframework.web.bind.annotation.ResponseBody;
          
          @Controller
          @EnableAutoConfiguration
          public class HomeController {
          
              @RequestMapping("/")
              public @ResponseBody String greeting() {
                  return "Hello World";
              }
          
          }`,
        setup: `
          // @config: reference spring-boot

          package hello;
          
          import org.springframework.boot.SpringApplication;
          import org.springframework.boot.autoconfigure.SpringBootApplication;
          
          @SpringBootApplication
          public class Application {
          
              public static void main(String[] args) {
                  SpringApplication.run(Application.class, args);
              }
          }
        `,
        fixture: `
          package hello;
          import static org.assertj.core.api.Assertions.assertThat;

          import org.junit.Test;
          import org.junit.runner.RunWith;
          import org.springframework.beans.factory.annotation.Autowired;
          import org.springframework.boot.test.context.SpringBootTest;
          import org.springframework.test.context.junit4.SpringRunner;
          
          @RunWith(SpringRunner.class)
          @SpringBootTest
          public class SmokeTest {
          
              @Autowired
              private HomeController controller;
          
              @Test
              public void contexLoads() throws Exception {                 
                  assertThat(controller).isNotNull();
              }
          }`
      }, function(buffer) {
        console.log(buffer.stdout);
        console.log(buffer.stderr);
        expect(buffer.stdout).to.contain('<LOG::-Startup Logs>');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed\n');
        done();
      });
    });
  });
});
