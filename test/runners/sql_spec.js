var expect = require('chai').expect;
var runner = require('../runner');

var itemsData = `
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
`

var query = 'SELECT * FROM items ORDER BY price DESC';

var itemsFixture = `
describe :items do
   it "should return 3 items" do
    expect(run_sql.to_a.count).to eq 3
   end
end
`

describe('sql runner', function () {
    runner.assertCodeExamples('sql');

    describe('.run', function () {
        
        describe('solution only', function() {
            it("should support sqlite", function(done) {
                runner.run({
                    language: 'sql',
                    languageVersion: 'sqlite',
                    setup: itemsData,
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
                    setup: itemsData,
                    code: query
                }, function(buffer) {
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
                    setup: itemsData,
                    code: query,
                    fixture: itemsFixture
                }, function(buffer) {
                    expect(buffer.stdout).to.contain('PASSED');
                    done();
                });
            });

            // it("should support postgres", function(done) {
            //     runner.run({
            //         language: 'sql',
            //         languageVersion: 'postgres',
            //         setup: itemsData,
            //         code: query,
            //         fixture: itemsFixture
            //     }, function(buffer) {
            //         expect(buffer.stdout).to.contain('PASSED');
            //         done();
            //     });
            // });
        });
    });
});