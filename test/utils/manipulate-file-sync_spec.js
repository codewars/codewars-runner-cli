const expect = require('chai').expect,
      manipulateFileSync = require('../../lib/utils/manipulate-file-sync');

describe('manipulateFileSync', function() {
  it("should support keeps", function() {
    const result = manipulateFileSync('/runner/frameworks/java/build.gradle', '/workspace/build.gradle', {
      keeps: [{
        target: /^dependencies {\n(  .*\n)*}/gm,
        select: '^  compile.*$keep.*\n',
        values: ['junit', 'lombok']
      }]
    });

    expect(result).to.not.contain("dom4j");
    expect(result).to.contain("repositories {\n  jcenter()");
  });
});
