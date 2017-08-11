"use strict";

const expect = require('chai').expect;
const runner = require('../runner');
const exec = require('child_process').exec;

describe('java runner', function() {
  before(function startDaemon(done) {
    this.timeout(0);
    exec('gradle --daemon --offline --quiet test', {
      cwd: '/runner/frameworks/gradle',
    }, (err) => {
      if (err) return done(err);
      console.log('Started Gradle daemon');
      done();
    });
  });

  describe('.run', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should handle basic code evaluation', function(done) {
      this.timeout(0);
      runner.run({
        language: 'java',
        code:`
          class Solution {
             public static void main(String[] args){
                  System.out.println("42");
             }
          }`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('42\n');
        done();
      });
    });

    it('should handle basic code evaluation on a custom class', function(done) {
      this.timeout(0);
      runner.run({
        language: 'java',
        code:`
          class Challenge {
             public static void main(String[] args){
                  System.out.println("42");
             }
          }`
      }, function(buffer) {
        expect(buffer.stdout).to.contain('42\n');
        done();
      });
    });
  });

  describe('junit', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });
    it('should handle basic junit tests', function(done) {
      this.timeout(0);
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
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle junit tests failing', function(done) {
      this.timeout(0);
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
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });

    it('should report junit messages', function(done) {
      this.timeout(0);
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
        expect(buffer.stdout).to.contain('<ERROR::>');
        expect(buffer.stdout).to.contain('null');
        done();
      });
    });

    it('should handle custom class names', function(done) {
      this.timeout(0);
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
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle packages', function(done) {
      this.timeout(0);
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
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle support setup code', function(done) {
      this.timeout(0);
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
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should handle support split files', function(done) {
      this.timeout(0);
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
        console.log(buffer);
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });

  describe('spring', function() {
    afterEach(function cleanup(done) {
      exec('rm -rf /home/codewarrior/project', function(err) {
        if (err) return done(err);
        done();
      });
    });

    it('should support smoke testing', function(done) {
      // from github.com/spring-guides/gs-testing-web
      this.timeout(0);
      runner.run({
        language: 'java',
        solution: [
          `package hello;`,
          ``,
          `import org.springframework.stereotype.Controller;`,
          `import org.springframework.web.bind.annotation.RequestMapping;`,
          `import org.springframework.web.bind.annotation.ResponseBody;`,
          ``,
          `@Controller`,
          `public class HomeController {`,
          `  @RequestMapping("/")`,
          `  public @ResponseBody String greeting() {`,
          `    return "Hello World!";`,
          `  }`,
          `}`,
        ].join('\n'),
        setup: [
          `package hello;`,
          ``,
          `import org.springframework.boot.SpringApplication;`,
          `import org.springframework.boot.autoconfigure.SpringBootApplication;`,
          ``,
          `@SpringBootApplication`,
          `public class Application {`,
          `  public static void main(String[] args) {`,
          `    SpringApplication.run(Application.class, args);`,
          `  }`,
          `}`,
        ].join('\n'),
        fixture: [
          `package hello;`,
          ``,
          `import static org.assertj.core.api.Assertions.assertThat;`,

          `import org.junit.Test;`,
          `import org.junit.runner.RunWith;`,
          `import org.springframework.beans.factory.annotation.Autowired;`,
          `import org.springframework.boot.test.context.SpringBootTest;`,
          `import org.springframework.test.context.junit4.SpringRunner;`,
          ``,
          ``,
          `@RunWith(SpringRunner.class)`,
          `@SpringBootTest`,
          `public class SmokeTest {`,
          ``,
          `    @Autowired`,
          `    private HomeController controller;`,
          ``,
          `    @Test`,
          `    public void contexLoads() throws Exception {`,
          `        assertThat(controller).isNotNull();`,
          `    }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should support http request testing', function(done) {
      // from github.com/spring-guides/gs-testing-web
      this.timeout(0);
      runner.run({
        language: 'java',
        solution: [
          `package hello;`,
          ``,
          `import org.springframework.stereotype.Controller;`,
          `import org.springframework.web.bind.annotation.RequestMapping;`,
          `import org.springframework.web.bind.annotation.ResponseBody;`,
          ``,
          `@Controller`,
          `public class HomeController {`,
          `  @RequestMapping("/")`,
          `  public @ResponseBody String greeting() {`,
          `    return "Hello World!";`,
          `  }`,
          `}`,
        ].join('\n'),
        setup: [
          `package hello;`,
          ``,
          `import org.springframework.boot.SpringApplication;`,
          `import org.springframework.boot.autoconfigure.SpringBootApplication;`,
          ``,
          `@SpringBootApplication`,
          `public class Application {`,
          `  public static void main(String[] args) {`,
          `    SpringApplication.run(Application.class, args);`,
          `  }`,
          `}`,
        ].join('\n'),
        fixture: [
          `package hello;`,
          ``,
          `import static org.assertj.core.api.Assertions.assertThat;`,

          `import org.junit.Test;`,
          `import org.junit.runner.RunWith;`,
          `import org.springframework.beans.factory.annotation.Autowired;`,
          `import org.springframework.boot.context.embedded.LocalServerPort;`,
          `import org.springframework.boot.test.context.SpringBootTest;`,
          `import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;`,
          `import org.springframework.boot.test.web.client.TestRestTemplate;`,
          `import org.springframework.test.context.junit4.SpringRunner;`,
          ``,
          ``,
          `@RunWith(SpringRunner.class)`,
          `@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)`,
          `public class HttpRequestTest {`,
          ``,
          `    @LocalServerPort`,
          `    private int port;`,
          ``,
          `    @Autowired`,
          `    private TestRestTemplate restTemplate;`,
          ``,
          `    @Test`,
          `    public void greetingShouldReturnDefaultMessage() throws Exception {`,
          `        assertThat(this.restTemplate.getForObject("http://localhost:" + port + "/", String.class)).contains("Hello World");`,
          `    }`,
          `}`,
        ].join('\n'),
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });
  });
});
