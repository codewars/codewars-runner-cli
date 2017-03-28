var expect = require('chai').expect;
var runner = require('../runner');


describe('gas runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation (no libc)', function(done) {
      runner.run({
        language: 'gas',
        code: [
          '   .global _start',
          '   .text',
          '_start:',
          '   mov     $1, %rax',
          '   mov     $1, %rdi',
          '   mov     $message, %rsi',
          '   mov     $8, %rdx',
          '   syscall',
          '   mov     $60, %rax',
          '   xor     %rdi, %rdi',
          '   syscall',
          'message:',
          '   .ascii "PV = nRT"'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('PV = nRT');
        done();
      });
    });
    it('should handle basic code evaluation (with libc)', function(done) {
      runner.run({
        language: 'gas',
        code: [
          '   .global  main',
          '   .text',
          'main:',
          '   mov     $message, %rdi',
          '   call    puts',
          '   ret',
          'message:',
          '   .asciz "Gas works with libc, too"'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Gas works with libc, too\n');
        done();
      });
    });
  });
});
