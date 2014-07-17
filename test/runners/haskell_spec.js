var expect = require('chai').expect;
var runner = require('../../lib/runners/haskell');


describe('haskell runner', function () {
    describe('.run', function () {
        it('should handle basic code evaluation', function (done) {
            runner.run({language: 'haskell',
                solution: 'main = putStrLn "42"'
            }, function (buffer) {
                expect(buffer.stdout).to.equal('42\n');
                done();
            });
        });
        it('should handle running a module with imports', function (done) {
            runner.run({
                language: 'haskell',
                solution: [
                    'module Foo where',
                    'import Control.Monad (filterM)',
                    'powerset :: [a] -> [[a]]',
                    'powerset = filterM (const [True,False])',
                    'main :: IO ()',
                    'main = putStr $ show $ powerset [1..3]'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]');
                done();
            });
        });
        it('should handle setup code', function (done) {
            runner.run({
                language: 'haskell',
                solution: [
                    'module Foo where',
                    'import Foo.Utils as Setup (powerset)',
                    'main :: IO ()',
                    'main = putStr $ show $ Setup.powerset [1..3]'
                ].join('\n'),
                setup: [
                    'module Foo.Utils (powerset) where',
                    'import Control.Monad (filterM)',
                    'powerset :: [a] -> [[a]]',
                    'powerset = filterM (const [True,False])'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]');
                done();
            });
        });
        it('should handle skipping module declaration', function (done) {
            runner.run({
                language: 'haskell',
                solution: [
                    'import Foo.Utils as Setup (powerset)',
                    'main :: IO ()',
                    'main = putStr $ show $ Setup.powerset [1..3]'
                ].join('\n'),
                setup: [
                    'module Foo.Utils (powerset) where',
                    'import Control.Monad (filterM)',
                    'powerset :: [a] -> [[a]]',
                    'powerset = filterM (const [True,False])'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.equal('[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]');
                done();
            });
        });
    });

    describe('codewars test framework (hspec)', function () {
        it('should be able to run a basic test', function (done) {
            runner.run({
                language: 'haskell',
                solution: 'module Foo where',
                fixture: [
                    'module Basic.Test where',
                    'import Test.CodeWars',
                    'main :: IO ()',
                    'main = test $ do',
                    '  describe "Prelude.head" $ do',
                    '    it "returns the first element of a list" $ do',
                    '      head [23 ..] `shouldBe` (23 :: Int)'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
                expect(buffer.stdout).to.contain('<IT::>returns the first element of a list');
                expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                done();
            });
        });
        it("should work even if test module name isn't specified", function (done) {
            runner.run({
                language: 'haskell',
                solution: 'module Foo where',
                fixture: [
                    'import Test.CodeWars',
                    'main :: IO ()',
                    'main = test $ do',
                    '  describe "Prelude.head" $ do',
                    '    it "returns the first element of a list" $ do',
                    '      head [23 ..] `shouldBe` (23 :: Int)'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
                expect(buffer.stdout).to.contain('<IT::>returns the first element of a list');
                expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                done();
            });
        });
        it("should work be able to import the solution", function (done) {
            runner.run({
                language: 'haskell',
                solution: [
                    'module CodeWars.Solution where',
                    'x :: Int',
                    'x = 1'
                ].join('\n'),
                fixture: [
                    'import CodeWars.Solution (x)',
                    'import Test.CodeWars',
                    'main :: IO ()',
                    'main = test $ do',
                    '  describe "x" $ do',
                    '    it "is 1" $ do',
                    '      x `shouldBe` (1 :: Int)'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('<IT::>is 1');
                expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                done();
            });
        });
        it("should be able to import the solution even when solution module name is unspecified (the default is module name for the solution is 'Main')", function (done) {
            runner.run({
                language: 'haskell',
                solution: [
                    'x :: Int',
                    'x = 1'
                ].join('\n'),
                fixture: [
                    'module Basic.Test where',
                    'import Main (x)',
                    'import Test.CodeWars',
                    'main :: IO ()',
                    'main = test $ do',
                    '  describe "x" $ do',
                    '    it "is 1" $ do',
                    '      x `shouldBe` (1 :: Int)'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('<IT::>is 1');
                done();
            });
        });
        it("should report when something is wrong", function (done) {
            runner.run({
                language: 'haskell',
                solution: 'x = 1',
                fixture: [
                    'module Sad.Path.Test where',
                    'import Test.CodeWars',
                    'import Main (x)',
                    'main = test $ do',
                    '  describe "x" $ do',
                    '    it "is 2" $ do',
                    '      x `shouldBe` 2'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('<IT::>is 2');
                expect(buffer.stdout).to.contain('<FAILED::>expected: 2 but got: 1');
                done();
            });
        });
        it("should print as a side effect", function (done) {
            runner.run({
                language: 'haskell',
                solution: 'x = do putStrLn "Test" ; return 1',
                fixture: [
                    'module PrintEffect where',
                    'import Test.CodeWars',
                    'import Main (x)',
                    'main = test $ do',
                    '  describe "x" $ do',
                    '    it "prints and returns 1" $ do',
                    '      xval <- x',
                    '      xval `shouldBe` 1',
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('Test\n<IT::>prints and returns 1');
                expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
                done();
            });
        });
        it("should fail fast", function (done) {
            runner.run({
                language: 'haskell',
                solution: 'x = 1',
                fixture: [
                    'module Fast.Fail.Test where',
                    'import Test.CodeWars',
                    'import Main (x)',
                    'main = test $ do',
                    '  describe "x" $ do',
                    '    it "is not really 2" $ do',
                    '      x `shouldBe` 2',
                    '    it "should never get here" $ do',
                    '      x `shouldBe` 3'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('<IT::>is not really 2');
                expect(buffer.stdout).to.contain('<FAILED::>expected: 2 but got: 1');
                expect(buffer.stdout).to.not.contain('<IT::>should never get here');
                expect(buffer.stdout).to.not.contain('<FAILED::>expected: 3 but got: 1');
                done();
            });
        });
        it("should report exceptions as errors", function (done) {
            runner.run({
                language: 'haskell',
                solution: 'x = head []',
                fixture: [
                    'module Fast.Fail.Test where',
                    'import Test.CodeWars',
                    'import Main (x)',
                    'main = test $ do',
                    '  describe "exception" $ do',
                    '    it "should throw" $ do',
                    '      x `shouldBe` 2'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>exception');
                expect(buffer.stdout).to.contain('<IT::>should throw');
                expect(buffer.stdout).to.contain('<ERROR::>ErrorCall (Prelude.head: empty list)');
                done();
            });
        });
    });
});