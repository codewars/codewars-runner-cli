"use strict";

const expect = require('chai').expect;
const runner = require('../runner');

describe('swift runner', function() {
  describe('examples', function() {
    runner.assertCodeExamples('swift');
  });

  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'swift', code: 'print(42)'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });

    it('should handle setup code', function(done) {
      runner.run({
        language: 'swift',
        languageVersion: '3',
        setup: "func foo() -> Int { return 999; }",
        code: "print(foo())"
      }, function(buffer) {
        expect(buffer.stdout).to.equal("999\n");
        done();
      });
    });

    it('should handle line output when there is setup code', function(done) {
      runner.run({
        language: 'swift',
        languageVersion: '3',
        setup: "func foo() -> Int { \nreturn 999; \n}",
        code: "print(foo(1))"
      }, function(buffer) {
        expect(buffer.stderr).to.contain("main.swift:1:11: error: argument passed to call that takes no arguments\nprint(foo(1))");
        done();
      });
    });
  });

  describe('xctest', function() {
    it('should handle a single assertion', function(done) {
      var code = `
                    class Calculator {
                        func add(a:Int, b:Int) -> Int {
                            return a + b
                        }
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class CalculatorTest: XCTestCase {
                              var calc : Calculator!

                              static var allTests = [
                                    ("testAddCheck", testAddCheck),
                              ]

                              override func setUp() {
                                super.setUp()
                                calc = Calculator()
                              }

                              func testAddCheck() {
                                XCTAssertEqual(calc.add(a:1, b:2), 3, "calc.add(1, 2) should be 3")
                              }
                            }

                            XCTMain([
                                testCase(CalculatorTest.allTests)
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>CalculatorTest');
        expect(buffer.stdout).to.contain('<IT::>testAddCheck');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
        done();
      });
    });

    it('should allow whitespace in "XCTMain ("', function(done) {
      var code = `
                    func add(_ a: Int, _ b: Int) -> Int {
                      return a + b
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class AddTest: XCTestCase {
                              static var allTests = [
                                    ("testAddCheck", testAddCheck),
                              ]
                              func testAddCheck() {
                                XCTAssertEqual(add(1, 1), 2, "add(1, 1) should be 2")
                              }
                            }

                            XCTMain  ([
                                testCase(AddTest.allTests)
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>AddTest');
        expect(buffer.stdout).to.contain('<IT::>testAddCheck');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
        done();
      });
    });

    it('should replace newlines in error messages with <:LF:>', function(done) {
      var code = `
                    class Calculator {
                        func add(a:Int, b:Int) -> Int {
                            // Force an error
                            return -1
                        }
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class CalculatorTest: XCTestCase {
                              static var allTests = [
                                    ("testAddCheck", testAddCheck),
                              ]

                              func testAddCheck() {
                                let calc = Calculator()
                                XCTAssertEqual(calc.add(a:1, b:2), 3, "calc.add(1, 2) \\n should be 3")
                              }
                            }

                            XCTMain([
                                testCase(CalculatorTest.allTests)
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<FAILED::>XCTAssertEqual failed: ("-1") is not equal to ("3") - calc.add(1, 2) <:LF:> should be 3\n');
        done();
      });
    });

    it('should handle multiple assertions', function(done) {
      var code = `
                    class Calculator {
                        func add(a:Int, b:Int) -> Int {
                            return a + b
                        }

                        func sub(a:Int, b:Int) -> Int {
                            // Let's force a test failure
                            return 12345
                        }

                        func mul(a:Int, b:Int) -> Int {
                            return a * b
                        }
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class CalculatorTest: XCTestCase {
                              var calc : Calculator!

                              static var allTests = [
                                    ("testAddCheck", testAddCheck),
                                    ("testSubCheck", testSubCheck),
                                    ("testMulCheck", testMulCheck)
                              ]

                              override func setUp() {
                                super.setUp()
                                calc = Calculator()
                              }

                              func testAddCheck() {
                                XCTAssertEqual(calc.add(a:1, b:2), 3, "calc.add(1, 2) should be 3")
                              }

                              func testSubCheck() {
                                XCTAssertEqual(calc.sub(a:3, b:4), -1, "calc.sub(3, 4) should be -1")
                              }

                              func testMulCheck() {
                                XCTAssertEqual(calc.mul(a:4, b:5), 20, "calc.mul(4, 5) should be 20")
                              }
                            }

                            XCTMain([
                                testCase(CalculatorTest.allTests)
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>CalculatorTest');
        expect(buffer.stdout).to.contain('<IT::>testAddCheck');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
        expect(buffer.stdout).to.contain('<IT::>testSubCheck');
        expect(buffer.stdout).to.contain('<FAILED::>XCTAssertEqual failed');
        expect(buffer.stdout).to.contain('<IT::>testMulCheck');
        done();
      });
    });

    it('should display test results for multiple assertions in the same test method', function(done) {
      var code = `
                    class Calculator {
                        func add(a:Int, b:Int) -> Int {
                            return a + b
                        }

                        func sub(a:Int, b:Int) -> Int {
                            // Let's force a test failure
                            return 12345
                        }

                        func mul(a:Int, b:Int) -> Int {
                            return a * b
                        }
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class CalculatorTest: XCTestCase {

                              static var allTests = [
                                    ("testCalculator", testCalculator),
                              ]

                              func testCalculator() {
                                let calc = Calculator()
                                XCTAssertEqual(calc.add(a:1, b:2), 3, "calc.add(1, 2) should be 3")
                                XCTAssertEqual(calc.sub(a:3, b:4), -1, "calc.sub(3, 4) should be -1")
                              }
                            }

                            XCTMain([
                                testCase(CalculatorTest.allTests)
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>CalculatorTest');
        expect(buffer.stdout).to.contain('<IT::>testCalculator');
        expect(buffer.stdout).not.to.contain('<PASSED::>Test Passed');
        expect(buffer.stdout).to.contain('<FAILED::>XCTAssertEqual failed:');
        expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
        done();
      });
    });

    it('should handle strings', function(done) {
      var code = `
                    class Greetings {
                        func sayHello() -> String {
                            return "hello, codewars!"
                        }
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class GreetingsTest: XCTestCase {
                              var greetings : Greetings!


                              static var allTests = [
                                    ("testSayHello", testSayHello),
                              ]

                              override func setUp() {
                                super.setUp()
                                greetings = Greetings()
                              }

                              func testSayHello() {
                                XCTAssertEqual(greetings.sayHello(), "hello, codewars!", "greetings.sayHello() should return \\"hello, codewars!\\"")
                              }
                            }

                            XCTMain([
                                testCase(GreetingsTest.allTests)
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>GreetingsTest');
        expect(buffer.stdout).to.contain('<IT::>testSayHello');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        expect(buffer.stdout).to.contain('<COMPLETEDIN::>');
        done();
      });
    });

    it('should handle multiple test cases', function(done) {
      var code = `
                    class Calculator {
                        func add(a:Int, b:Int) -> Int {
                            return a + b
                        }
                    }

                    class Person {
                        let name: String

                        init(_ name: String) {
                            self.name = name
                        }

                        func greet(_ other: String) -> String {
                            return "Hello, \(other), I am \(name), it's nice to meet you!"
                        }
                    }
                `;
      runner.run({
        language: 'swift',
        languageVersion: '3',
        code: code,
        fixture: `
                            import XCTest
                            class CalculatorTest: XCTestCase {
                              static var allTests = [("testAddCheck", testAddCheck)]

                              func testAddCheck() {
                                let calc = Calculator()
                                XCTAssertEqual(calc.add(a:1, b:2), 3, "calc.add(1, 2) should be 3")
                              }
                            }

                            class PersonTest: XCTestCase {
                                static var allTests = [("testGreet", testGreet)]

                                func testGreet() {
                                    let person = Person("jorge")
                                    XCTAssertEqual(person.greet("Aditya"), "Hello, Aditya, I am Jorge, it's nice to meet you!")
                                }
                            }

                            XCTMain([
                                testCase(CalculatorTest.allTests),
                                testCase(PersonTest.allTests),
                            ])
                        `,
        testFramework: 'xctest'
      }, function(buffer) {
        expect(buffer.stdout).to.contain('<DESCRIBE::>CalculatorTest');
        expect(buffer.stdout).to.contain('<IT::>testAddCheck');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        expect(buffer.stdout).to.contain('<DESCRIBE::>PersonTest');
        expect(buffer.stdout).to.contain('<IT::>testGreet');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
  });
});
