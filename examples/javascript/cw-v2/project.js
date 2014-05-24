var __fs = require( 'fs' ), __vm = require('vm');
function __include(path) {
    var code = __fs.readFileSync(path, 'utf-8');
    __vm.runInThisContext(code, path);
}
require('../../../frameworks/javascript/cw-js-v2');

__include('./solution');
(function() {
    __include('./fixture');
})();

