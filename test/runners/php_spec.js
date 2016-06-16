var expect = require('chai').expect;
var runner = require('../runner');


describe( 'php runner', function(){
    describe( '.run', function(){
        runner.assertCodeExamples('php');

        it( 'should handle basic code evaluation', function(done){
            runner.run({language: 'php', solution: 'echo 42;'}, function(buffer) {
                expect(buffer.stdout).to.equal('42');
                done();
            });
        });

        it( 'should handle bad syntax', function(done){
            runner.run({
            	language: 'php', 
            	solution: `
            		fliggaflagam!!!
        		`
        	}, function(buffer) {
                expect(buffer.stderr).to.contain('syntax error');
                done();
            });
        });

        it( 'should handle undefined functions', function(done){
            runner.run({
            	language: 'php', 
            	solution: `
            		fliggaflagam();
        		`
        	}, function(buffer) {
                expect(buffer.stderr).to.contain('Uncaught Error');
                done();
            });
        });

        it( 'should handle the latest and greatest of PHP 7', function(done){
            runner.run({
            	language: 'php', 
            	solution: `
            		function sumOfInts(int ...$ints)
					{
					    return array_sum($ints);
					}
					echo sumOfInts(2, '3', 4.1);
        		`
        	}, function(buffer) {
                expect(buffer.stdout).to.equal('9');
                done();
            });
        });

        describe('cw-2', function() {
        	it('should handle some basic tests', function(done) {
		        runner.run({
	                language: 'php',
	                code: `
				      function double($a) {
				        return $a * 2;
				      }
	                `,
	                fixture: `
	                	$test->assert_equals(double(1, 2), 2);
	                `,
	                testFramework: 'cw-2'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<PASSED::>');
	                done();
	            });
        	});

        	it('should be able to reference preloaded code', function(done) {
		        runner.run({
	                language: 'php',
	                setup: `
	                	class SomeClass
						{
						    const CONSTANT = 42;
						}
	                `,
	                code: `
					    function theConstant() {
					        return SomeClass::CONSTANT;
					    }
	                `,
	                fixture: `
	                	$test->assert_equals(theConstant(), SomeClass::CONSTANT);
	                `,
	                testFramework: 'cw-2'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<PASSED::>');
	                done();
	            });
        	});

        	it('should handle failed tests', function(done) {
		        runner.run({
	                language: 'php',
	                code: `
				      function double($a) {
				        return $a * 2;
				      }
	                `,
	                fixture: `
	                	$test->assert_equals(double(1), 6);
	                `,
	                testFramework: 'cw-2'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<FAILED::>');
	                done();
	            });
        	});

        	it('should handle bad assertions', function(done) {
		        runner.run({
	                language: 'php',
	                code: `
						const CONSTANT = 42;
	                `,
	                fixture: `
	                	$test->assert_equals(CONSTANT, 'apples');
	                `,
	                testFramework: 'cw-2'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<FAILED::>');
	                done();
	            });
        	});
        });

        describe('phpunit', function() {
        	it('should handle some basic tests', function(done) {
		        runner.run({
	                language: 'php',
	                code: `
				      function double($a) {
				        return $a * 2;
				      }
	                `,
	                fixture: `
			            class DoubleMethod extends TestCase
			            {
		                	public function testDouble() {
		                		$this->assertEquals(double(1), 2);
		                	}
			            }
	                `,
	                testFramework: 'phpunit'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<PASSED::>');
	                done();
	            });
        	});

        	it('should be able to reference preloaded code', function(done) {
		        runner.run({
	                language: 'php',
	                setup: `
	                	class SomeClass
						{
						    const CONSTANT = 42;
						}
	                `,
	                code: `
					    function theConstant() {
					        return SomeClass::CONSTANT;
					    }
	                `,
	                fixture: `
			            class TheConstantMethod extends TestCase
			            {
		                	public function testConstantMethod() {
	                			$this->assertEquals(theConstant(), SomeClass::CONSTANT);
		                	}
			            }
	                `,
	                testFramework: 'phpunit'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<PASSED::>');
	                done();
	            });
        	});

        	it('should handle failed tests', function(done) {
		        runner.run({
	                language: 'php',
	                code: `
				      function double($a) {
				        return $a * 2;
				      }
	                `,
	                fixture: `
			            class TheConstantMethod extends TestCase
			            {
		                	public function testConstantMethod() {
	                			$this->assertEquals(double(1), 6);
		                	}
			            }
	                `,
	                testFramework: 'phpunit'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<FAILED::>');
	                done();
	            });
        	});

        	it('should handle bad assertions', function(done) {
		        runner.run({
	                language: 'php',
	                code: `
						const CONSTANT = 42;
	                `,
	                fixture: `
			            class TheConstantMethod extends TestCase
			            {
		                	public function testConstantMethod() {
	                			$this->assertEquals(CONSTANT, 'apples');
		                	}
			            }
	                `,
	                testFramework: 'phpunit'
	            },
	            function(buffer) {
	                expect(buffer.stdout).to.contain('<FAILED::>');
	                done();
	            });
        	});
    	});

    });
});
