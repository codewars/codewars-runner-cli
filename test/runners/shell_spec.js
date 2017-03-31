var expect = require('chai').expect;
var runner = require('../runner');


describe('shell runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'shell', code: 'echo 42'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
    describe('rspec', function() {
      it('should support run_shell', function(done) {
        runner.run({
          language: 'shell',
          code: 'echo $1',
          fixture: `
                      describe "solution" do
                        it "should return 42" do
                           result = run_shell args: [42]
                           expect(result.strip).to eq "42"
                        end
                      end
                    `
        }, function(buffer) {
          console.log(buffer.stdout);
          expect(buffer.stdout).to.contain('<PASSED::>');
          done();
        });
      });
    });
  });
});
