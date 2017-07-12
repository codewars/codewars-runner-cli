var fs = require('fs'),
    expect = require('chai').expect,
    yaml = require('js-yaml'),
    runner = require('../lib/runner');

module.exports.run = runner.run;

// loops through the language specific examples within the examples.yml file and calls the cb function with each individual example
function iterateCodeExamples(language, cb) {
  var examples = yaml.safeLoad(fs.readFileSync('./examples/' + language + '.yml', 'utf8'));
  if (examples) {
    for (var framework in examples) {
      for (var example in examples[framework]) {
        cb(framework, example, examples[framework][example]);
      }
    }
  }
}

module.exports.assertCodeExamples = function(language, version) {
  describe('example challenges', function() {
    iterateCodeExamples(language, function(framework, name, example) {
      it('should define an initial code block', function() {
        expect(example.initial).to.be.a('string');
      });

      it('should have a passing ' + name + ' example', function(done) {
        runner.run({
          language: language,
          languageVersion: version,
          setup: example.setup,
          code: example.answer,
          fixture: example.fixture,
          testFramework: framework
        }, function(buffer) {
          expect(buffer.stdout).to.not.contain('<FAILED::>');
          expect(buffer.stdout).to.not.contain('<ERROR::>');
          if (buffer.stderr) console.log(buffer.stderr);
          done();
        });
      });
    });
  });
};
