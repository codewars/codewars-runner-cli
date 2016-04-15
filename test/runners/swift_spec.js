if (process.platform == 'darwin') {
    var expect = require('chai').expect;
    var runner = require('../runner');


    describe('swift runner', function () {
        describe('.run', function () {
            it('should handle basic code evaluation', function (done) {
                runner.run({
                    language: 'swift',
                    code: "println(\"You've baked a really lovely cake, but then you've used dog shit for frosting. - Steve Jobs (commenting on an employee's program)\")"
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal("You've baked a really lovely cake, but then you've used dog shit for frosting. - Steve Jobs (commenting on an employee's program)\n");
                    done();
                });
            });
            it('should handle setup code', function (done) {
                runner.run({
                    language: 'swift',
		    setup: "func foo() -> Int { return 999; }",
                    code: "println(foo())"
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal("999\n");
                    done();
                });
            });
            it('should handle a full test', function (done) {
                runner.run({
                    language: 'swift',
                    code: [
			'// payload1.swift',
			'func foo() ->  Int { return 999; }'
		    ].join('\n'),
                    fixture: [
			'// fixture1.swift',
                        'import XCTest',
	        	'class runner_test: XCTestCase {',
	                '    func testExample() {',
	                '        XCTAssert(foo() == 998 , "badbadbad")',
	                '    }',
	        	'}'
                    ].join('\n')
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.contain("badbadbad");
                    done();
                });
            });
            it('should handle a broken test', function (done) {
                runner.run({
                    language: 'swift',
                    code: [
			'// payload2.swift',
			'func foo() ->'
		    ].join('\n'),
                    fixture: [
			'// fixture2.swift',
                        'import XCTest',
	        	'class runner_test: XCTestCase {',
	                '    func testExample() {',
	                '        XCTAssert(foo() == 998 , "badbadbad")',
	                '    }',
	        	'}'
                    ].join('\n')
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.contain("payload2.swift:2:14: error: expected type for function result");
                    done();
                });
            });

            it('should handle a full with setup', function (done) {
                runner.run({
                    language: 'swift',
		    setup: [
			'// bar.swift',
			'func bar() ->  Int { return 999; }'
		    ].join('\n'),
                    code: [
			'// payload3.swift',
			'func foo() ->  Int { return bar(); }'
		    ].join('\n'),
                    fixture: [
			'// fixture3.swift',
                        'import XCTest',
	        	'class setup_test: XCTestCase {',
	                '    func testFooBar() {',
	                '        XCTAssert(foo() == 999 , "badbadbad")',
	                '    }',
	        	'}'
                    ].join('\n')
                }, function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.contain("<PASSED::>Test Case '-[setup_test testFooBar]' passed");
                    done();
                });
            });

        });
    });
}
