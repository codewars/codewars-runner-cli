var expect = require('chai').expect;
var runner = require('../runner');
var exec = require('child_process').exec;

describe( 'go runner', function(){
    describe( '.run', function(){
        it( 'should handle basic code evaluation', function(done){
            var solution = ['package main',
                'import "fmt"',
                'func main() {',
                '    fmt.Println("42")',
                '}',
                ''].join('\n');

            runner.run({language: 'go', code: solution}, function(buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
    });
});


describe('testing with Ginkgo', function() {
    afterEach(function cleanup(done) {
        exec('rm -rf /home/codewarrior/go/src/codewarrior', function(err, stdout, stderr) {
            if (err) return done(err);
            done();
        });
    });

    it('should handle basic code assertion', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution

func Add(a, b int) int {
  return a + b
}
`,
              fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
        }, function(buffer) {
            expect(buffer.stdout).to.contain('<PASSED::>');
            done();
        });
    });

    it('should handle basic code assertion failure', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution

func Add(a, b int) int {
  return a - b
}
`,
            fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
        }, function(buffer) {
             expect(buffer.stdout).to.contain('<FAILED::>');
             done();
        });
    });

    it('should handle mixed success and failure', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution

func Add(a, b int) int {
  return a - b
}
func Sub(a, b int) int {
  return a - b
}
`,
            fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
var _ = Describe("Sub", func() {
  It("should subtract integers", func() {
    Expect(Sub(1, 1)).To(BeZero())
  })
})
`
        }, function(buffer) {
             expect(buffer.stdout).to.contain('<FAILED::>');
             expect(buffer.stdout).to.contain('<PASSED::>');
             done();
        });
    });

    it('should handle It with mixed result', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution

func Add(a, b int) int {
  return a - b
}
`,
            fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(0, 0)).To(Equal(0))
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
        }, function(buffer) {
             expect(buffer.stdout).to.contain('<FAILED::>');
             expect(buffer.stdout).not.to.contain('<PASSED::>');
             done();
        });
    });

    it('should handle panic', function(done) {
        runner.run({
            language: 'go',
            solution: [
                'package solution',
                '',
                'func Add(a, b int) int {',
                '  panic("!")', // 4
                '  return a - b',
                '}'
            ].join('\n'),
            fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
        }, function(buffer) {
             expect(buffer.stdout).to.contain('<ERROR::>');
             expect(buffer.stdout).to.contain('src/codewarrior/solution/solution.go:4');
             done();
        });
    });

    it('should handle nested describes', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution

func Add(a, b int) int {
  return a + b
}
func Sub(a, b int) int {
  return a - b
}
`,
              fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Solution", func() {
  Describe("Add", func() {
    It("should add positive integers", func() {
      Expect(Add(1, 1)).To(Equal(2))
    })
    It("should add negative integers", func() {
      Expect(Add(-1, -1)).To(Equal(-2))
    })
  })

  Describe("Sub", func() {
    It("should subtract positive integers", func() {
      Expect(Sub(1, 1)).To(Equal(0))
    })
    It("should subtract negative integers", func() {
      Expect(Sub(-1, -1)).To(Equal(0))
    })
  })
})
`
        }, function(buffer) {
            expect(buffer.stdout).to.contain('<PASSED::>');
            const expected = [
              '<DESCRIBE::>',
              '  <DESCRIBE::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '  <COMPLETEDIN::>',
              '  <DESCRIBE::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '  <COMPLETEDIN::>',
              '<COMPLETEDIN::>'
            ].join('').replace(/\s/g, '');
            expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
            done();
        });
    });

    it('should allow test contexts with same name', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution

func Add(a, b int) int {
  return a + b
}
func Sub(a, b int) int {
  return a - b
}
`,
              fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Solution", func() {
  Describe("Add", func() {
    It("should add positive integers", func() {
      Expect(Add(1, 1)).To(Equal(2))
    })
    It("should add negative integers", func() {
      Expect(Add(-1, -1)).To(Equal(-2))
    })
  })

  Describe("Sub", func() {
    It("should subtract positive integers", func() {
      Expect(Sub(1, 1)).To(Equal(0))
    })
    It("should subtract negative integers", func() {
      Expect(Sub(-1, -1)).To(Equal(0))
    })
  })
})

var _ = Describe("Solution", func() {
  Describe("Add", func() {
    It("should add two positive integers", func() {
      Expect(Add(1, 1)).To(Equal(2))
    })
    It("should add two negative integers", func() {
      Expect(Add(-1, -1)).To(Equal(-2))
    })
  })

  Describe("Sub", func() {
    It("should subtract two positive integers", func() {
      Expect(Sub(1, 1)).To(Equal(0))
    })
    It("should subtract two negative integers", func() {
      Expect(Sub(-1, -1)).To(Equal(0))
    })
  })
})
`
        }, function(buffer) {
            expect(buffer.stdout).to.contain('<PASSED::>');
            const expected = [
              '<DESCRIBE::>',
              '  <DESCRIBE::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '  <COMPLETEDIN::>',
              '  <DESCRIBE::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '  <COMPLETEDIN::>',
              '<COMPLETEDIN::>',
              '<DESCRIBE::>',
              '  <DESCRIBE::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '  <COMPLETEDIN::>',
              '  <DESCRIBE::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '    <IT::><PASSED::><COMPLETEDIN::>',
              '  <COMPLETEDIN::>',
              '<COMPLETEDIN::>'
            ].join('').replace(/\s/g, '');
            expect(buffer.stdout.match(/<(?:DESCRIBE|IT|PASSED|COMPLETEDIN)::>/g).join('')).to.equal(expected);
            done();
        });
    });

    it('should allow arbitrary package name', function(done) {
        runner.run({
            language: 'go',
            solution: `
package kata

func Add(a, b int) int {
  return a + b
}
`,
              fixture: `
package kata_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/kata"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
        }, function(buffer) {
            expect(buffer.stdout).to.contain('<PASSED::>');
            done();
        });
    });

    it('should allow solution to log', function(done) {
        runner.run({
            language: 'go',
            solution: `
package solution
import "fmt"

func Add(a, b int) int {
  fmt.Println(a)
  fmt.Printf("b = %d\\n", b)
  return a + b
}
`,
              fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
        }, function(buffer) {
            expect(buffer.stdout).to.contain('<PASSED::>');
            expect(buffer.stdout).to.contain('1');
            expect(buffer.stdout).to.contain('b = 1');
            done();
        });
    });
});


describe('Gomega matchers', function() {
    afterEach(function cleanup(done) {
        exec('rm -rf /home/codewarrior/go/src/codewarrior', function(err, stdout, stderr) {
            if (err) return done(err);
            done();
        });
    });

    describe('Equal(x)', function() {
        it('should provide descriptive error message', function(done) {
            runner.run({
                language: 'go',
                solution: `
package solution

func Add(a, b int) int {
  return a - b
}
`,
                fixture: `
package solution_test

import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/solution"
)

var _ = Describe("Add", func() {
  It("should add integers", func() {
    Expect(Add(1, 1)).To(Equal(2))
  })
})
`
            }, function(buffer) {
                 expect(buffer.stdout).to.contain('<FAILED::>');
                 expect(buffer.stdout).to.contain('Expected<:LF:>    <int>: 0<:LF:>to equal<:LF:>    <int>: 2');
                 done();
            });
        });
    });

    describe('BeEmpty()', function() {
        it('should provide descriptive error message', function(done) {
            runner.run({
                language: 'go',
                solution: `
package empty

func Empty() []int {
  return []int{1}
}
`,
              fixture: `
package empty_test
import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/empty"
)

var _ = Describe("Empty", func() {
  It("should be empty", func() {
    Expect(Empty()).To(BeEmpty())
  })
})
`
            }, function(buffer) {
                expect(buffer.stdout).to.contain('<FAILED::>');
                expect(buffer.stdout).to.contain('Expected<:LF:>    <[]int | len:1, cap:1>: [1]<:LF:>to be empty');
                done();
            });
        });
    });

    describe('HaveLen(count)', function() {
        it('should provide descriptive error message', function(done) {
            runner.run({
                language: 'go',
                solution: `
package pair

func Pair(a, b int) []int {
  return []int{a}
}
`,
              fixture: `
package pair_test
import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/pair"
)

var _ = Describe("Pair", func() {
  It("should have length of 2", func() {
    Expect(Pair(1, 2)).To(HaveLen(2))
  })
})
`
            }, function(buffer) {
                expect(buffer.stdout).to.contain('<FAILED::>');
                expect(buffer.stdout).to.contain('Expected<:LF:>    <[]int | len:1, cap:1>: [1]<:LF:>to have length 2');
                done();
            });
        });
    });

    describe('HaveCap(count)', function() {
        it('should provide descriptive error message', function(done) {
            runner.run({
                language: 'go',
                solution: `
package pair

func Pair(a, b int) []int {
  return []int{a}
}
`,
              fixture: `
package pair_test
import (
  . "github.com/onsi/ginkgo"
  . "github.com/onsi/gomega"
  . "codewarrior/pair"
)

var _ = Describe("Pair", func() {
  It("should have length of 2", func() {
    Expect(Pair(1, 2)).To(HaveCap(2))
  })
})
`
            }, function(buffer) {
                expect(buffer.stdout).to.contain('<FAILED::>');
                expect(buffer.stdout).to.contain('Expected<:LF:>    <[]int | len:1, cap:1>: [1]<:LF:>to have capacity 2');
                done();
            });
        });
    });
});

