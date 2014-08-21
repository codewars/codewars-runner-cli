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
                    'import Test.Hspec',
                    'main :: IO ()',
                    'main = hspec $ do',
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
                    'import Test.Hspec',
                    'main :: IO ()',
                    'main = hspec $ do',
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
                    'import Test.Hspec',
                    'main :: IO ()',
                    'main = hspec $ do',
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
                    'import Test.Hspec',
                    'main :: IO ()',
                    'main = hspec $ do',
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
                    'import Test.Hspec',
                    'import Main (x)',
                    'main = hspec $ do',
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
                    'import Test.Hspec',
                    'import Main (x)',
                    'main = hspec $ do',
                    '  describe "x" $ do',
                    '    it "prints and returns 1" $ do',
                    '      xval <- x',
                    '      xval `shouldBe` 1'
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
                    'import Test.Hspec',
                    'import Main (x)',
                    'main = hspec $ do',
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
                    'import Test.Hspec',
                    'import Main (x)',
                    'main = hspec $ do',
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
    describe('potpourri', function () {
        it('can handle SQLite interaction', function (done) {
            runner.run({language: 'haskell',
                solution: [
                    '{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}',
                    '{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}',
                    '{-# LANGUAGE NoMonomorphismRestriction #-}',
                    'module Movies where',
                    'import Database.Persist (insertMany)',
                    'import Database.Persist.Sqlite (runSqlite, runMigration)',
                    'import Database.Persist.TH (mkPersist, mkMigrate, persistUpperCase, share, sqlSettings)',
                    'share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistUpperCase|',
                    'Movies',
                    '   title    String',
                    '   year     Int',
                    '   rating   Int',
                    '   deriving Eq Show',
                    '|]',
                    'mkMoviesDB :: IO ()',
                    'mkMoviesDB = runSqlite "/tmp/movies.db" $ do',
                    '  runMigration migrateTables',
                    '  insertMany',
                    '    [ Movies "Rise of the Planet of the Apes" 2011 77',
                    '    , Movies "Dawn of the Planet of the Apes" 2014 91',
                    '    , Movies "Alien" 1979 97',
                    '    , Movies "Aliens" 1986 98',
                    '    , Movies "Mad Max" 1979 95',
                    '    , Movies "Mad Max 2: The Road Warrior" 1981 100',
                    '    ]',
                    '  return ()'
                ].join('\n'),
                fixture: [
                    '{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}',
                    '{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}',
                    '{-# LANGUAGE NoMonomorphismRestriction #-}',
                    'import Test.Hspec',
                    'import Database.Persist',
                    'import Control.Monad.IO.Class (MonadIO(liftIO))',
                    'import Database.Persist.Sqlite (runSqlite)',
                    'import Database.Persist.Sql (rawQuery)',
                    'import Data.Conduit (($$), (=$))',
                    'import Data.Conduit.List as CL',
                    'import Data.Text (unpack)',
                    'import Movies (mkMoviesDB)',

                    'data Movie = Movie String Integer Integer deriving (Eq, Show)',

                    'getMovies :: IO [Movie]',
                    'getMovies = runSqlite "/tmp/movies.db" $ do',
                    '  rawQuery "select Title, Year, Rating from Movies" [] $$ CL.map toMovie =$ consume',
                    '  where',
                    '    toMovie [PersistText title, PersistInt64 year, PersistInt64 rating] =',
                    '      Movie (unpack title) (toInteger year) (toInteger rating)',

                    'main :: IO ()',
                    'main = hspec $ do',
                    '  describe "/tmp/movies.db" $ do',
                    '    it "contains the movies we expect" $ do',
                    '      mkMoviesDB',
                    '      movies <- getMovies',
                    '      liftIO $ movies `shouldBe` [ Movie "Rise of the Planet of the Apes" 2011 77',
                    '                                 , Movie "Dawn of the Planet of the Apes" 2014 91',
                    '                                 , Movie "Alien" 1979 97,Movie "Aliens" 1986 98',
                    '                                 , Movie "Mad Max" 1979 95',
                    '                                 , Movie "Mad Max 2: The Road Warrior" 1981 100]'
                ].join('\n')
            }, function (buffer) {
                expect(buffer.stdout).to.contain('Test Passed');
                done();
            });
        });
    });
});
