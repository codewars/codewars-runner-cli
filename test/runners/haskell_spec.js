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

    describe( 'codewars test framework', function(){
        it('should be able to run a basic test', function(done){
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
            }, function(buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
                expect(buffer.stdout).to.contain('<PASSED::> - returns the first element of a list\n');
                done();
            });
        });
        it("should work even if test module name isn't specified", function(done){
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
            }, function(buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
                expect(buffer.stdout).to.contain('<PASSED::> - returns the first element of a list\n');
                done();
            });
        });
        it("should work be able to import the solution", function(done){
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
            }, function(buffer) {
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('<PASSED::> - is 1\n');
                done();
            });
        });
        it("should be able to import the solution even when solution module name is unspecified (the default is module name for the solution is 'Main')", function(done){
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
            }, function(buffer) {
                console.log(buffer.stderr);
                expect(buffer.stdout).to.contain('<DESCRIBE::>x');
                expect(buffer.stdout).to.contain('<PASSED::> - is 1\n');
                done();
            });
        });
    });
});
