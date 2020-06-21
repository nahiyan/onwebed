var convert = require('xml-js')
var fs = require('fs')

function compile (filePath) {
  fs.readFile(filePath, 'utf8', (err, data) => {
    if (err) {
      console.log(`Error: Failed to read file ${filePath} for compilation.`)
    } else {
      console.log(convert.xml2js(data))
    }
  })
}

module.exports.compile = compile
