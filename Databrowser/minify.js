var concat = require('concat')
var minifier = require('minifier')
var fs = require('fs');

// Returns an array of all of the JS files in a directory
var getFiles = function(folder, ext) {
  var items = (fs.readdirSync(folder));
  var files = [];
  var folders = [];
  // For each item in the directory decide if it's a file or a folder
  for (var i=0; i<items.length; i++) {
    if (items[i].indexOf('.') === -1) {
      folders.push(items[i])
    } else if (items[i].indexOf(ext) >= 0) {
      files.push(folder + '/' + items[i])
    }
  }
  // For each folder call getJS. Recursion!
  for (var i=0; i<folders.length; i++) {
    files = files.concat(getFiles(folder + '/' + folders[i], ext));
  }
  return files
}

// combine every js file in the JS directory
concat(getFiles('js', '.js'), 'build.js', function (error) { 

  // now minify that file
  minifier.minify("./build.js")

});

// even though this is concating one file it compiles the CSS @imports
concat(['style/Mazama_databrowser.css'], 'build.css', function (error) { 

  // now minify that file
  minifier.minify("./build.css")

});



