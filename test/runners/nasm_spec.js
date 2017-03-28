var expect = require('chai').expect;
var runner = require('../runner');


describe('nasm runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation (no libc)', function(done) {
      runner.run({
        language: 'nasm',
        code: [
          '   global  _start',
          '   section .text',
          '_start:',
          '   mov     rax, 1',
          '   mov     rdi, 1',
          '   mov     rsi, message',
          '   mov     rdx, 25',
          '   syscall',
          '   mov     eax, 60',
          '   xor     rdi, rdi',
          '   syscall',
          'message:',
          'db      "Hello, Netwide Assembler!", 25'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Hello, Netwide Assembler!');
        done();
      });
    });
    it('should handle basic code evaluation (with libc)', function(done) {
      runner.run({
        language: 'nasm',
        code: [
          '   global  main',
          '   extern  puts',
          '   section .text',
          'main:',
          '   mov     rdi, message',
          '   call    puts',
          '   ret',
          'message:',
          'db      "Netwide Assembler together with LIBC!  Let\'s Port Codewars From Rails to THIS! \\m/", 0'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('Netwide Assembler together with LIBC!  Let\'s Port Codewars From Rails to THIS! \\m/\n');
        done();
      });
    });
  });
});
