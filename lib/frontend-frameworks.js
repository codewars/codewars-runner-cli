// Simple map of includes to Karma file globs, so end-users don't need to know how to load
// specific libraries
// TODO: replace with a smarter way to download and use libraries on the fly
var frameworks = {
  'angular@1.2': [
    '/runner/frameworks/javascript/angular/1.2.9/angular.js',
    '/runner/frameworks/javascript/angular/1.2.9/angular-{mocks,resource,route,sanitize}.js',
  ],
  'angular@1.3': [
    '/runner/frameworks/javascript/angular/1.3.9/angular.js',
    '/runner/frameworks/javascript/angular/1.3.9/angular-{mocks,resource,route,sanitize}.js',
  ],
  'angular@1.4': [
    '/runner/frameworks/javascript/angular/1.4.9/angular.js',
    '/runner/frameworks/javascript/angular/1.4.9/angular-{mocks,resource,route,sanitize}.js',
  ],
  'angular@1.5': [
    '/runner/frameworks/javascript/angular/1.5.8/angular.js',
    '/runner/frameworks/javascript/angular/1.5.8/angular-{mocks,resource,route,sanitize}.js',
  ],
};

module.exports.find = function find(name) {
  var result = [name];
  if (frameworks[name]) {
    result = frameworks[name];
    if (!Array.isArray(result)) {
      result = [result];
    }
  }
  return result;
};
