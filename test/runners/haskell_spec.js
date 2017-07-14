var expect = require('chai').expect,
    runner = require('../runner');

describe('haskell runner', function() {
  describe('.run', function() {
    runner.assertCodeExamples('haskell');

    it('should handle basic code evaluation', function(done) {
      runner.run({
        language: 'haskell',
        code: 'main = putStrLn "42"'
      }, function(buffer) {
        expect(buffer.stdout).to.equal('42\n');
        done();
      });
    });
    it('should handle running a module with imports', function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module Foo where',
          'import Control.Monad (filterM)',
          'powerset :: [a] -> [[a]]',
          'powerset = filterM (const [True,False])',
          'main :: IO ()',
          'main = putStr $ show $ powerset [1..3]'
        ].join('\n')
      }, function(buffer) {
        expect(buffer.stdout).to.equal('[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]');
        done();
      });
    });
    it('should handle setup code', function(done) {
      runner.run({
        language: 'haskell',
        code: [
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
      }, function(buffer) {
        expect(buffer.stdout).to.equal('[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]');
        done();
      });
    });
    it('should handle skipping module declaration', function(done) {
      runner.run({
        language: 'haskell',
        code: [
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
      }, function(buffer) {
        expect(buffer.stdout).to.equal('[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]');
        done();
      });
    });
  });

  describe('codewars test framework (hspec)', function() {
    it('should be able to run a basic test', function(done) {
      runner.run({
        language: 'haskell',
        code: 'module Foo where',
        fixture: [
          'module Basic.Test where',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Prelude.head" $ do',
          '    it "returns the first element of a list" $ do',
          '      head [23 ..] `shouldBe` (23 :: Int)'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
        expect(buffer.stdout).to.contain('<IT::>returns the first element of a list');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should work even if test module name isn't specified", function(done) {
      runner.run({
        language: 'haskell',
        code: 'module Foo where',
        fixture: [
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Prelude.head" $ do',
          '    it "returns the first element of a list" $ do',
          '      head [23 ..] `shouldBe` (23 :: Int)'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
        expect(buffer.stdout).to.contain('<IT::>returns the first element of a list');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should work be able to import the code", function(done) {
      runner.run({
        language: 'haskell',
        code: [
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
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>x');
        expect(buffer.stdout).to.contain('<IT::>is 1');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should be able to import the code even when code module name is unspecified (the default is module name for the code is 'Main')", function(done) {
      runner.run({
        language: 'haskell',
        code: [
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
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>x');
        expect(buffer.stdout).to.contain('<IT::>is 1');
        done();
      });
    });
    it("should report when something is wrong", function(done) {
      runner.run({
        language: 'haskell',
        code: 'x = 1',
        fixture: [
          'module Sad.Path.Test where',
          'import Test.Hspec',
          'import Main (x)',
          'main = hspec $ do',
          '  describe "x" $ do',
          '    it "is 2" $ do',
          '      x `shouldBe` 2'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>x');
        expect(buffer.stdout).to.contain('<IT::>is 2');
        expect(buffer.stdout).to.contain('<FAILED::>expected: 2');
        expect(buffer.stdout).to.contain('but got: 1');
        done();
      });
    });
    it("should print as a side effect", function(done) {
      runner.run({
        language: 'haskell',
        code: 'x = do putStrLn "Test" ; return 1',
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
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>x');
        expect(buffer.stdout).to.contain('Test\n<IT::>prints and returns 1');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should fail fast", function(done) {
      runner.run({
        language: 'haskell',
        code: 'x = 1',
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
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>x');
        expect(buffer.stdout).to.contain('<IT::>is not really 2');
        expect(buffer.stdout).to.contain('<FAILED::>expected: 2');
        expect(buffer.stdout).to.contain('but got: 1');
        expect(buffer.stdout).to.not.contain('<IT::>should never get here');
        expect(buffer.stdout).to.not.contain('<FAILED::>expected: 3 but got: 1');
        done();
      });
    });
    it("should report exceptions as errors", function(done) {
      runner.run({
        language: 'haskell',
        code: 'x = head []',
        fixture: [
          'module Fast.Fail.Test where',
          'import Test.Hspec',
          'import Main (x)',
          'main = hspec $ do',
          '  describe "exception" $ do',
          '    it "should throw" $ do',
          '      x `shouldBe` 2'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('<DESCRIBE::>exception');
        expect(buffer.stdout).to.contain('<IT::>should throw');
        expect(buffer.stdout).to.contain('<ERROR::>ErrorCall (Prelude.head: empty list)');
        done();
      });
    });
    it("should be able to hide a module from the code code", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'x :: Int',
          'x = 1'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (x)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a module (happy path)" $ do',
          '    it "Data.Monoid is hidden" $ do',
          '      hidden $ Module \"Data.Monoid\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Data.Monoid is hidden');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should fail if a module which is supposed to be hidden is not", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.Monoid',
          'x :: Int',
          'x = 1'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (x)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a module (sad path)" $ do',
          '    it "Data.Monoid is not hidden!" $ do',
          '      hidden $ Module \"Data.Monoid\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Data.Monoid is not hidden!');
        expect(buffer.stdout).to.contain('<FAILED::>Import declarations must hide Data.Monoid');
        done();
      });
    });
    it("should recognize when things are hidden from a particular module", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.List hiding (reverse)',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function (happy path)" $ do',
          '    it "Data.List.reverse is hidden" $ do',
          '      hidden $ FromModule \"Data.List\" \"reverse\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Data.List.reverse is hidden');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should fail when a symbol from a module that ought to be hidden is not", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.List (intercalate)',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function (sad path)" $ do',
          '    it "Data.List.intercalate is not hidden!" $ do',
          '      hidden $ FromModule \"Data.List\" \"intercalate\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Data.List.intercalate is not hidden!');
        expect(buffer.stdout).to.contain('<FAILED::>Import declarations must hide Data.List.intercalate');
        done();
      });
    });
    it("should fail when a symbol from a module that ought to be hidden is not because the whole module was imported", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.List',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function (sad path)" $ do',
          '    it "Data.List.intercalate is not hidden, because the whole module was imported!" $ do',
          '      hidden $ FromModule \"Data.List\" \"intercalate\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Data.List.intercalate is not hidden, because the whole module was imported!');
        expect(buffer.stdout).to.contain('<FAILED::>Import declarations must hide Data.List.intercalate');
        done();
      });
    });
    it("should fail when a symbol from a module that ought to be hidden is not because it wasn't hidden properly", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.List hiding (reverse)',
          'import Control.Monad hiding ((=<<))',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function (sad path)" $ do',
          '    it "Control.Monad.>=> is not hidden, because we forgot!" $ do',
          '      hidden $ FromModule \"Control.Monad\" \">=>\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Control.Monad.>=> is not hidden, because we forgot!');
        expect(buffer.stdout).to.contain('<FAILED::>Import declarations must hide Control.Monad.>=>');
        done();
      });
    });
    it("should fail when a function is hidden once in a module, but later imported", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.List hiding (reverse)',
          'import Control.Monad hiding ((>=>))',
          'import Control.Monad ((>=>))',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function (sad path)" $ do',
          '    it "Control.Monad.>=> is not hidden, because we imported it after all!" $ do',
          '      hidden $ FromModule \"Control.Monad\" \">=>\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Control.Monad.>=> is not hidden, because we imported it after all!');
        expect(buffer.stdout).to.contain('<FAILED::>Import declarations must hide Control.Monad.>=>');
        done();
      });
    });
    it("should fail when a function is hidden once in a module, but later imported as qualified", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Data.List hiding (reverse)',
          'import Control.Monad hiding ((>=>))',
          'import qualified Control.Monad as WhatMonadRhymesWith',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function (sad path)" $ do',
          '    it "Control.Monad.>=> is not hidden, because we imported Control.Monad qualified!" $ do',
          '      hidden $ FromModule \"Control.Monad\" \">=>\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Control.Monad.>=> is not hidden, because we imported Control.Monad qualified!');
        expect(buffer.stdout).to.contain('<FAILED::>Import declarations must hide Control.Monad.>=>');
        done();
      });
    });
    it("should be able to hide Prelude functions", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'import Prelude hiding (reverse)',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function from Prelude (happy path)" $ do',
          '    it "Prelude.reverse is hidden" $ do',
          '      hidden $ FromModule \"Prelude\" \"reverse\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Prelude.reverse is hidden');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
    it("should detect when we failed to hide a Prelude function", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'module CodeWars.Solution where',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'import CodeWars.Solution (reverse)',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function from Prelude (sad path)" $ do',
          '    it "Prelude.reverse is NOT hidden!" $ do',
          '      hidden $ FromModule \"Prelude\" \"reverse\"'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Prelude.reverse is NOT hidden!');
        expect(buffer.stdout).to.contain('<FAILED::>');
        done();
      });
    });
    it("should detect when we hid a Prelude function, even when we forgot to say our module name", function(done) {
      runner.run({
        language: 'haskell',
        code: [
          'import Prelude hiding (reverse)',
          'reverse :: [a] -> [a]',
          'reverse = foldl (flip (:)) []'
        ].join('\n'),
        fixture: [
          'module Default.Module.Name.Is.Main.Test where',
          'import qualified Main',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '  describe "Testing BlackListing a particular function from Prelude, when module name is not specified (happy path)" $ do',
          '    it "Prelude.reverse is indeed hidden" $ do',
          '      hidden [FromModule \"Prelude\" \"reverse\"]',
          '  describe "Main.reverse reverses a thing" $ do',
          '    it "Main.reverse is functionally the same as Prelude.reverse" $ do',
          '      let testInput = [1..10]',
          '      Main.reverse testInput `shouldBe` reverse testInput',
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Prelude.reverse is indeed hidden');
        expect(buffer.stdout).to.contain('Main.reverse is functionally the same as Prelude.reverse');
        expect(buffer.stdout).to.contain('<PASSED::>');
        done();
      });
    });

    it('should not warn when tabs are used (Codewars/codewars.com#700)', function(done) {
      runner.run({
        language: 'haskell',
        code: 'module Foo where',
        fixture: [
          'module Basic.Test where',
          'import Test.Hspec',
          'main :: IO ()',
          'main = hspec $ do',
          '\tdescribe "Prelude.head" $ do',
          '\t\tit "returns the first element of a list" $ do',
          '\t\t\thead [23 ..] `shouldBe` (23 :: Int)'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stderr).to.equal('');
        expect(buffer.stdout).to.contain('<DESCRIBE::>Prelude.head');
        expect(buffer.stdout).to.contain('<IT::>returns the first element of a list');
        expect(buffer.stdout).to.contain('<PASSED::>Test Passed');
        done();
      });
    });
  });
  describe('haskell', function() {
    it('can handle SQLite interaction', function(done) {
      runner.run({
        language: 'haskell',
        code: [
          '{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}',
          '{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}',
          '{-# LANGUAGE NoMonomorphismRestriction, GeneralizedNewtypeDeriving #-}',
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
          'import Data.Text (pack, unpack)',
          'import Movies (mkMoviesDB)',
          'import Control.Monad (when)',
          'import System.Posix.Files (fileExist)',
          'import System.Directory (removeFile)',

          'data Movie = Movie String Integer Integer deriving (Eq, Show)',

          'moviesDBFileName :: String',
          'moviesDBFileName = "/tmp/movies.db"',

          'getMovies :: IO [Movie]',
          'getMovies = runSqlite (pack moviesDBFileName) $ do',
          '  rawQuery "select Title, Year, Rating from Movies" [] $$ CL.map toMovie =$ consume',
          '  where',
          '    toMovie [PersistText title, PersistInt64 year, PersistInt64 rating] =',
          '      Movie (unpack title) (toInteger year) (toInteger rating)',

          'deleteIfExists :: String -> IO ()',
          'deleteIfExists fileName = do',
          '  exists <- fileExist fileName',
          '  when exists $ removeFile fileName',

          'main :: IO ()',
          'main = hspec $ do',
          '  describe "/tmp/movies.db" ',
          '  $ before (deleteIfExists moviesDBFileName)',
          '  $ after_ (deleteIfExists moviesDBFileName)',
          '  $ do',
          '    it "contains the movies we expect" $ do',
          '      mkMoviesDB',
          '      movies <- getMovies',
          '      liftIO $ movies `shouldBe` [ Movie "Rise of the Planet of the Apes" 2011 77',
          '                                 , Movie "Dawn of the Planet of the Apes" 2014 91',
          '                                 , Movie "Alien" 1979 97,Movie "Aliens" 1986 98',
          '                                 , Movie "Mad Max" 1979 95',
          '                                 , Movie "Mad Max 2: The Road Warrior" 1981 100]'
        ].join('\n')
      }, function(buffer) {
        console.log(buffer);
        expect(buffer.stdout).to.contain('Test Passed');
        done();
      });
    });
  });
});
