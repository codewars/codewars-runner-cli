var expect = require('chai').expect;
var runner = require('../../lib/runners/arm');


describe('arm runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation (no libc)', function (done) {
            runner.run({language: 'arm',
                    solution: [
                        '.data',
                        'msg:',
                        '.ascii "Armed and dangerous :D"',
                        'len = . - msg',
                        '   .text',
                        '_start:',
                        '   mov     $1, %rax',
                        '   mov     $1, %rdi',
                        '   mov     $message, %rsi',
                        '   mov     $8, %rdx',
                        '   syscall',
                        '   mov     $60, %rax',
                        '   xor     %rdi, %rdi',
                        '   syscall'
                    ].join('\n')},
                function (buffer) {
                    expect(buffer.stdout).to.equal('PV = nRT');
                    done();
                });
        });
        it('should handle basic code evaluation (with libc)', function (done) {
            runner.run({language: 'arm',
                    solution: [
                        '   .global  main',
                        '   .text',
                        'main:',
                        '   mov     $message, %rdi',
                        '   call    puts',
                        '   ret',
                        'message:',
                        '   .asciz "arm works with libc, too"'
                    ].join('\n')},
                function (buffer) {
                    expect(buffer.stdout).to.equal('arm works with libc, too\n');
                    done();
                });
        });
    });
});
