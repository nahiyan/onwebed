var convert = require('xml-js')
var fs = require('fs')

var processed = 'fuck'

function processBox (element) {
  // if (element.attributes !== undefined) {
  // }
}

function processLiquidBox (element) {
  processed += 'liquid box'
}
function processSolidBox (element) {}

function processDocumentBody (element) {
  if (element.elements !== undefined) {
    element.elements.forEach(element => {
      switch (element.name) {
        case 'liquid_box':
          processLiquidBox(element)
          break
        default:
          processSolidBox(element)
          break
      }
    })
  }
}

function processRootElements (elements) {
  elements.forEach(element => {
    switch (element.name) {
      case 'document_body':
        processDocumentBody(element)
        break
      default:
        break
    }
  })
}

function compile (filePath, cb) {
  fs.readFile(filePath, 'utf8', (err, data) => {
    if (err) {
      console.error(`Error: Failed to read file ${filePath} for compilation.`)
      process.exit()
    } else {
      const documentObjectified = convert.xml2js(data)

      processed = ''
      processRootElements(documentObjectified.elements)
      cb(processed)
    }
  })
}

module.exports.compile = compile
