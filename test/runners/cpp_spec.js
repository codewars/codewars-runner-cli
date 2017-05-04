var expect = require('chai').expect;
var runner = require('../runner');

describe('cpp runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('cpp');

    it('should handle basic code evaluation', function(done) {
      var code = `
                #include <iostream>
                int main() {
                    printf("Hello World");
                }
           `;

      runner.run({language: 'cpp', code: code}, function(buffer) {
        expect(buffer.stdout).to.equal("Hello World");
        done();
      });
    });

    it('should handle C++11 nonsense', function(done) {
      var code = `
                #include "stdio.h"
                int main() {
                    auto f = []{ printf("Finally, lambdas in C++.  Now if we had typeclasses, purity and laziness we might have a reasonable functional programming language."); };
                    f();
                }
            `;

      runner.run({language: 'cpp', code: code}, function(buffer) {
        expect(buffer.stdout).to.equal("Finally, lambdas in C++.  Now if we had typeclasses, purity and laziness we might have a reasonable functional programming language.");
        done();
      });
    });

    it('should handle C++14 digit seperators', function(done) {
      var code = `
                #include <iostream>
                int main() {
                    int x = 10'000'000;
                    std::cout << x << std::endl;
                }
            `;

      runner.run({language: 'cpp', code: code}, function(buffer) {
        expect(buffer.stdout).to.equal("10000000\n");
        done();
      });
    });

    it('should handle compile errors', function(done) {
      var code = `
                int main() {
                    fudge();
                    doubleFudge();
                }
            `;

      runner.run({language: 'cpp', code: code}, function(buffer) {
        expect(buffer.stderr).to.contain("use of undeclared identifier \'fudge\'");
        expect(buffer.stderr).to.contain("use of undeclared identifier \'doubleFudge\'");
        expect(buffer.stderr).to.contain("2 errors generated.");
        done();
      });
    });

    it('should handle setup code and imports', function(done) {
      runner.run({
        language: 'cpp',
        setup: `
                    int square(int a) { return a * a ; }
                `,
        code: `
                    #include <iostream>
                    int main() {
                        std::cout << square(6);
                    }
                `
      }, function(buffer) {
        expect(buffer.stdout).to.equal('36');
        done();
      });
    });

    it('should handle importing classes and member functions', function(done) {
      runner.run({
        language: 'cpp',
        setup: `
                    #include <iostream>
                    class pizza {
                    public:
                        std::string tastes() {
                            return "good";
                        }
                    };
                `,
        code: `
                    #include <iostream>
                    int main() {
                        pizza p;
                        std::cout << p.tastes();
                    }
                `
      }, function(buffer) {
        expect(buffer.stdout).to.equal('good');
        done();
      });
    });

    describe('igloo bdd', function() {
      it('should handle basic assertions', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        unsigned int NumberOne() {
                            return 1;
                        }
                    `,
          fixture: `
                        Describe(basic_tests)
                        {
                          It(should_test_well)
                          {
                            Assert::That(NumberOne(), Equals(1));
                          }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should handle basic failures', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        unsigned int NumberOne() {
                            return 1;
                        }
                    `,
          fixture: `
                        Describe(basic_tests)
                        {
                          It(should_test_well)
                          {
                            Assert::That(NumberOne(), Equals(2));
                          }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.contain('Expected: equal to 2<:LF:>Actual: 1');
          done();
        });
      });

      it('should run test iterations in the order written (not alphabetical)', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        char LetterA() {
                            return 'a';
                        }
                        char LetterB() {
                            return 'b';
                        }
                        char LetterC() {
                            return 'c';
                        }
                    `,
          fixture: `
                        Describe(alphabetical_tests)
                        {
                          It(b_test)
                          {
                            Assert::That(LetterB(), Equals('b'));
                          }
                          It(c_test)
                          {
                            Assert::That(LetterC(), Equals('c'));
                          }
                          It(a_test)
                          {
                            Assert::That(LetterA(), Equals('a'));
                          }
                        };
                    `
        }, function(buffer) {
          var aIndex = buffer.stdout.indexOf("a_test");
          var bIndex = buffer.stdout.indexOf("b_test");
          var cIndex = buffer.stdout.indexOf("c_test");
          expect(bIndex).to.be.below(cIndex);
          expect(cIndex).to.be.below(aIndex);
          done();
        });
      });

      it('should record std output', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        unsigned int NumberOne() {
                            std::cout << "Hello Codewars!" << std::endl;
                            return 1;
                        }
                    `,
          fixture: `
                        Describe(basic_tests)
                        {
                          It(should_test_well)
                          {
                            Assert::That(NumberOne(), Equals(2));
                          }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('Hello Codewars!');
          done();
        });
      });

      it('should work with the example two oldest ages', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        #include <list>
                        #include <iostream>
                        using namespace std;

                        list<int> two_oldest_ages(list<int> ages) {
                            int oldest = 0, nextOldest;
                            for(auto& age:ages) {
                                if(age > oldest) {
                                    nextOldest = oldest;
                                    oldest = age;
                                }
                                else if(age > nextOldest) {
                                    nextOldest = age;
                                }
                            }
                            return list<int> {nextOldest, oldest};
                        }
                    `,
          fixture: `
                        list<int> results = two_oldest_ages({ 1, 5, 87, 45, 8, 8 });
                        Describe(two_oldest_ages_test)
                        {
                            It(should_return_the_oldest)
                            {
                                Assert::That(results.front(), Equals(45));
                            }
                            It(thing_inherit_from_base)
                            {
                                Assert::That(results.back(), Equals(87));
                            }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>two_oldest_ages_test');
          expect(buffer.stdout).to.contain('<IT::>should_return_the_oldest');
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.contain('<IT::>thing_inherit_from_base');
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should only display errors once', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        //
                    `,
          fixture: `
                        Describe(tests)
                        {
                            It(should_do_something_0)
                            {
                                Assert::That(1, Equals(2));
                            }
                            It(should_do_something_1)
                            {
                                Assert::That(123, Equals(456));
                            }
                            It(should_do_something_2)
                            {
                                Assert::That('a', Equals('b'));
                            }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout.match(/Expected: equal to 2/g).length).to.equal(1);
          expect(buffer.stdout.match(/Expected: equal to 456/g).length).to.equal(1);
          expect(buffer.stdout.match(/Expected: equal to b/g).length).to.equal(1);
          done();
        });
      });

      it('should work with the virtual bug fix example', function(done) {
        runner.run({
          language: 'cpp',
          code: `
                        struct Entity {
                            int run() {
                                return speed();
                            }
                            virtual int speed() {
                                return 5;
                            }
                        };

                        struct Player: public Entity {
                            virtual int speed() {
                                return 10;
                            }
                        };
                    `,
          fixture: `
                        Entity e;
                        Player p;

                        Describe(entity)
                        {
                            It(should_run_at_speed_5) {
                                Assert::That(e.run(), Equals(5));
                            }
                        };

                        Describe(player)
                        {
                            It(should_run_at_speed_10) {
                                Assert::That(p.run(), Equals(10));
                            }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>entity');
          expect(buffer.stdout).to.contain('<DESCRIBE::>player');
          expect(buffer.stdout).to.contain('<IT::>should_run_at_speed_5');
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.contain('<IT::>should_run_at_speed_10');
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should handle inheritance like a champ', function(done) {
        runner.run({
          language: 'cpp',
          setup: `
                        class Base
                        {
                        public:
                            virtual int big_number() = 0;
                            int member_var = 42;
                        };
                    `,
          code: `
                        class Thing: public Base
                        {
                        public:
                            int big_number() {
                                return 4200 + member_var;
                            }
                        };
                    `,
          fixture: `
                        Thing t;
                        Describe(inheritance_tests)
                        {
                            It(should_access_the_base_var)
                            {
                                Assert::That(t.member_var, Equals(42));
                            }
                            It(thing_inherit_from_base)
                            {
                                Assert::That(t.big_number(), Equals(4242));
                            }
                        };
                    `
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>inheritance_tests');
          expect(buffer.stdout).to.contain('<IT::>should_access_the_base_var');
          expect(buffer.stdout).to.contain('<PASSED::>');
          expect(buffer.stdout).to.contain('<IT::>thing_inherit_from_base');
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });

      it('should have output format commands on independent line', function(done) {
        runner.run({
          language: 'cpp',
          code: '//',
          fixture: [
            `Describe(foo) {`,
            ` It(bar) {`,
            `   std::cout << "foo";`,
            `   Assert::That(1, Equals(2));`,
            ` }`,
            `};`
          ].join('\n')
        }, function(buffer) {
          expect(buffer.stdout).to.contain("\n<FAILED::>");
          done();
        });
      });
    });
  });
});
