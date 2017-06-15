var expect = require('chai').expect;
var runner = require('../runner');


describe('crystal runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('crystal');

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'crystal',
        code: 'puts 42'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });

    describe('spec', function() {
      it('should handle a basic assertion', function(done) {
        runner.run({
          language: 'crystal',
          code: 'a = 1',
          fixture: [
            'describe "test" do',
            '  it("test2") { 1.should eq(1) }',
            'end',
          ].join('\n'),
          testFramework: 'spec'
        }, function(buffer) {
          expect(buffer.stdout).to.include('<DESCRIBE::>test\n\n<IT::>test2\n\n<PASSED::>');
          done();
        });
      });

      it('should handle a basic failed assertion', function(done) {
        runner.run({
          language: 'crystal',
          code: 'a = 1',
          fixture: [
            'describe "test" do',
            '  it("test2") { 1.should eq(2) }',
            'end',
          ].join('\n'),
          testFramework: 'spec'
        }, function(buffer) {
          expect(buffer.stdout).to.contain('<DESCRIBE::>test\n\n<IT::>test2');
          expect(buffer.stdout).to.contain('<FAILED::>');
          expect(buffer.stdout).to.not.contain('<PASSED::>');
          done();
        });
      });

      it('should handle errored code', function(done) {
        runner.run({
          language: 'crystal',
          code: 'A = 1',
          fixture: [
            'describe "test" do',
            '  it("test2") { A.should eq 1 }',
            '  it("test2") { A.idontexist() }',
            'end',
          ].join('\n'),
          testFramework: 'spec'
        }, function(buffer) {
          expect(buffer.stdout).to.eq('');
          expect(buffer.stderr).to.contain('undefined method');
          done();
        });
      });

      it('should have output format command on independent line', function(done) {
        runner.run({
          language: 'crystal',
          testFramework: 'spec',
          code: '#',
          fixture: [
            'describe "tests" do',
            '  it "test case" do',
            '    print "foo"',
            '    1.should eq 2',
            '  end',
            'end',
          ].join('\n'),
        }, function(buffer) {
          expect(buffer.stdout).to.contain('\n<FAILED::>');
          done();
        });
      });
    });
  });
});
