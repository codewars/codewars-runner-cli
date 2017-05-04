var expect = require('chai').expect;
var runner = require('../runner');


describe('fsharp runner', function() {
  describe('.run', function() {
    it('should handle basic code evaluation', function(done) {
      runner.run({language: 'fsharp', code: 'printfn "42"'}, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
  });

  describe('using Fuchu for testing', function() {
    runner.assertCodeExamples('fsharp');

    it('should be able to run a basic test', function(done) {
      runner.run({
        language: 'fsharp',
        code: [
          'type Person = {name: string};;',
          'let greet (person: Person) = "Hello, " + person.name + "!";;',
        ].join('\n'),
        fixture: [
          'module Tests = begin',
          '   open Fuchu',
          '   let suite = ',
          '       testList "Person" [',
          '           testCase ".greet" <| (fun _ -> ',
          '               let person: Person = { name = "Jack" } in ',
          '               Assert.Equal("Greet", "Hello, Jack!", (greet person))',
          '           )',
          '       ]',
          'end'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain("\n<IT::>Person/.greet\n\n<PASSED::>Person/.greet\n\n<COMPLETEDIN::>");
        done();
      });
    });
    it('should be able to handle a failing test', function(done) {
      runner.run({
        language: 'fsharp',
        code: [
          'type Person = {name: string};;',
          'let greet (person: Person) = "Hello, " + person.name + "!";;',
        ].join('\n'),
        fixture: [
          'module Tests = begin',
          '   open Fuchu',
          '   let suite = ',
          '       testList "Broken" [',
          '           testCase "test" <| (fun _ -> ',
          '               Assert.Equal("Broken test", true, false)',
          '           )',
          '       ]',
          'end'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain("\n<IT::>Broken/test\n\n<FAILED::><:LF:>Broken test");
        expect(buffer.stdout).to.contain("\n<COMPLETEDIN::>");
        done();
      });
    });

    it('should have output format command on independent line', function(done) {
      runner.run({
        language: 'fsharp',
        code: '//',
        fixture: [
          'module Tests = begin',
          '    open Fuchu',
          '    let suite =',
          '        testList "Tests" [',
          '            testCase "test" <| (fun _ ->',
          '                System.Console.Write("foo")',
          '                Assert.Equal("test 1", 1, 2)',
          '            )',
          '        ]',
          'end',
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.contain("\n<FAILED::>");
        done();
      });
    });
  });
});
