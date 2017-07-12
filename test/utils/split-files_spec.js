const expect = require('chai').expect,
      split = require('../../lib/utils/split-files');

describe('splitFiles', function() {
  it("should ignore files without splits", function() {
    const content = `
      this is root content
            
      this is Stuff.java
      
      this is Other.java
    `;

    const result = split(content);

    expect(result.root).to.contain('this is root content');
    expect(result.root).to.contain('this is Stuff.java');
    expect(result.splits).to.eq(0);
  });

  it("should split into multiple files", function() {
    const content = `
      this is root content
      
      // @config: split-file Stuff.java
      
      this is Stuff.java
      
      // @config:split-file Other.java
      
      this is Other.java
    `;

    const result = split(content);

    expect(result.root).to.contain('this is root content');
    expect(result.root).to.not.contain('this is Stuff.java');
    expect(result.splits).to.eq(2);
    expect(result.files['Stuff.java']).to.contain('this is Stuff.java');
    expect(result.files['Stuff.java']).to.not.contain('this is Other.java');
  });
});
