var expect = require('chai').expect;
var runner = require('../runner');

var itemsSetup = `
# @use-database spec
DB.drop_table :items rescue nil
DB.create_table :items do
  primary_key :id
  String :name
  Float :price
end

items = DB[:items] # Create a dataset

# Populate the table
items.insert(:name => 'a', :price => 10)
items.insert(:name => 'b', :price => 35)
items.insert(:name => 'c', :price => 20)
`;

var query = 'SELECT * FROM items ORDER BY price DESC';

var itemsFixture = `
describe :items do
   it "should return 3 items" do
    expect(run_sql.to_a.count).to eq 3
   end
end

puts "database = #{DATABASE}"
`;

describe('sql runner', function() {
  runner.assertCodeExamples('sql');

  describe('.run', function() {

    describe('solution only', function() {
      it("should support sqlite", function(done) {
        runner.run({
          language: 'sql',
          languageVersion: 'sqlite',
          setup: itemsSetup,
          code: query
        }, function(buffer) {
          expect(buffer.stdout).to.contain('"name":"c"');
          done();
        });
      });

      it("should support postgres", function(done) {
        runner.run({
          language: 'sql',
          languageVersion: 'postgres',
          setup: itemsSetup,
          code: query
        }, function(buffer) {
          console.log(buffer.stdout);
          expect(buffer.stdout).to.contain('"name":"c"');
          done();
        });
      });
    });

    describe("rspec", function() {
      it("should support sqlite", function(done) {
        runner.run({
          language: 'sql',
          languageVersion: 'sqlite',
          setup: itemsSetup,
          code: query,
          fixture: itemsFixture
        }, function(buffer) {
          expect(buffer.stdout).to.contain('PASSED');
          done();
        });
      });

      it("should support projectMode", function(done) {
        runner.run({
          language: 'sql',
          languageVersion: 'sqlite',
          files: {
            'spec.rb': `require './setup.rb'\n${itemsFixture}`,
            'setup.rb': `require "/runner/frameworks/ruby/sql"\n${itemsSetup}`,
            'solution.sql': query
          }
        }, function(buffer) {
          expect(buffer.stdout).to.contain('PASSED');
          done();
        });
      });

      it("should support postgres", function(done) {
        runner.run({
          language: 'sql',
          languageVersion: 'postgres',
          setup: itemsSetup,
          code: query,
          fixture: itemsFixture
        }, function(buffer) {
          expect(buffer.stdout).to.contain('PASSED');
          expect(buffer.stdout).to.contain('database = spec');
          done();
        });
      });
    });
  });
});

describe('Sequel deprecation warnings', function() {
  it("should be silenced", function(done) {
    runner.run({
      language: 'sql',
      languageVersion: 'sqlite',
      setup: [
        `DB.drop_table :items rescue nil`,
        `DB.create_table :items do`,
        `  primary_key :id`,
        `  String :name`,
        `  Int :age`,
        `end`,
        ``,
        `$items = DB[:items]`,
        `100.times do`,
        `  $items.insert(name: Faker::Name.name, age: Faker::Number.number(2))`,
        `end`,
      ].join('\n'),
      solution: `SELECT * FROM items WHERE age > 50`,
      fixture: [
        `describe :items do`,
        `   it "should return records with age over 50" do`,
        `     expect(run_sql.to_a.count).to eq DB[:items].where('age > 50').count`,
        `   end`,
        `end`,
      ].join('\n'),
    }, function(buffer) {
      expect(buffer.stdout).not.to.contain('SEQUEL DEPRECATION WARNING:');
      expect(buffer.stderr).to.be.empty;
      done();
    });
  });
});

