var expect = require('chai').expect;
var runner = require('../../lib/runners/arm');


describe('arm runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation (no libc)', function (done) {
            runner.run({language: 'arm',
                    solution: [
                        '.data',
                        'message: .ascii "ARMed and dangerous :D"',
                        'len = . - message',
                        '.text',
                        '.global _start',
                        '_start:',
                        '   mov     %r0, $1',
                        '   ldr     %r1, =message',
                        '   ldr     %r2, =len',
                        '   mov     %r7, $4',
                        '   swi     $0',
                        '   mov     %r0, $0',
                        '   mov     %r7, $1',
                        '   swi     $0'
                    ].join('\n')},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('ARMed and dangerous :D');
                    done();
                });
        });
        it('should not use libc even with alternate spelling of global as globl', function (done) {
            runner.run({language: 'arm',
                    solution: [
                        '.data',
                        'message: .ascii "Tokyo dialect"',
                        'len = . - message',
                        '.text',
                        '.globl _start',
                        '_start:',
                        '   mov     %r0, $1',
                        '   ldr     %r1, =message',
                        '   ldr     %r2, =len',
                        '   mov     %r7, $4',
                        '   swi     $0',
                        '   mov     %r0, $0',
                        '   mov     %r7, $1',
                        '   swi     $0'
                    ].join('\n')},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('Tokyo dialect');
                    done();
                });
        });
        it('should handle basic code evaluation (with libc)', function (done) {
            runner.run({language: 'arm',
                    solution: [
                        '.global  main',
                        'main:',
                        '   push {ip, lr}',
                        '   ldr  r0, =message',
                        '   bl   puts',
                        '   mov  r0, #0',
                        '   pop  {ip, pc}',
                        'message: .asciz "ARM works with libc, too"'
                    ].join('\n')},
                function (buffer) {
                    console.log(buffer);
                    expect(buffer.stdout).to.equal('ARM works with libc, too\n');
                    done();
                });
        });
    });
});
