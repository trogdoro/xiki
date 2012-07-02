// Sample js file this can invoke
/*
  @/projects/xiki_wrappers/
    - pie.js
      | exports.menu = function () {
      |   return "hey/\nyou/\nthere/\n";
      | };
*/

// Gets shelled out to by xiki to delegate call to a .js file.
// Just gets the args passed in and requires and invokes.
var file = process.argv[2]
var path = process.argv[3]

var clazz = require(file);
var output = clazz.menu();
console.log(output);
