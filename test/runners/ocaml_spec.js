var expect = require('chai').expect;
var runner = require('../runner');


describe('ocaml runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('ocaml');

    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'ocaml', code: 'print_string "42\n";;'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
  });

  describe('using oUnit for testing', function() {
    it('should be able to run a basic test', function(done) {
      runner.run({
        language: 'ocaml',
        code: [
          'module Person = struct',
          '    type t = { name: string }',
          '    let greet (person: t) = "Hello, " ^ person.name ^ "!"',
          'end'
        ].join('\n'),
        fixture: [
          'module Tests = struct',
          '   open OUnit',
          '   let suite = ',
          '       [',
          '           "Person" >:::',
          '           [',
          '               ".greet" >:: (fun _ -> ',
          '                   let jack: Person.t = { Person.name = "Jack" } in ',
          '                   assert_equal "Hello, Jack!" (Person.greet jack)',
          '               )',
          '           ]',
          '       ]',
          'end'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain("<DESCRIBE::>Person\n<IT::>.greet\n<PASSED::>");
        done();
      });
    });
  });
});
