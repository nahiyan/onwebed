const express = require('express')
const router = express.Router()
const document = require('../../../../lib/document/utilities')
const compiler = require('../../../../lib/document/compiler')
const path = require('path')
const xmlJs = require('xml-js')
const fs = require('fs')
const pretty = require('pretty')

/* GET home page. */
router.get('/', function (req, res, next) {
  const documentNames = fs
    .readdirSync(req.app.get('sourceDirectory'), 'utf8')
    .filter(document.isDocument)
    .map(function (file) {
      return path.basename(file, path.extname(file))
    })
  res.render('documents/index', {
    title: 'Onwebed - Documents',
    documentNames: documentNames,
    sourceDirectory: req.app.get('sourceDirectory'),
    destinationDirectory: req.app.get('destinationDirectory')
  })
})

// router.get('/new', function (req, res, next) {
//   const documentNames = fs
//     .readdirSync(req.app.get('sourceDirectory'), 'utf8')
//     .filter(document.isDocument)
//     .map(function (file) {
//       return path.basename(file, path.extname(file))
//     })
//   res.render('documents/index', {
//     title: 'Onwebed - Documents',
//     documentNames: documentNames,
//     sourceDirectory: req.app.get('sourceDirectory'),
//     destinationDirectory: req.app.get('destinationDirectory')
//   })
// })

// Edit Document
router.get('/edit/:name', function (req, res) {
  const name = req.params.name
  const content = xmlJs.xml2js(
    fs.readFileSync(
      path.join(req.app.get('sourceDirectory'), name + '.od'),
      'utf8'
    )
  )
  res.render('documents/edit', {
    content: content,
    name: name
  })
})

// Save Document
router.post('/save/:name', function (req, res) {
  const name = req.params.name
  const fileName = name + '.od'
  const document = req.body
  const markup = pretty(xmlJs.js2xml(document))

  // Write the file
  fs.writeFileSync(path.join(req.app.get('sourceDirectory'), fileName), markup)

  // Compile all documents, just in case they got dependencies
  compiler.compileFromDirectory(req.app.get('sourceDirectory'))(
    req.app.get('destinationDirectory')
  )()

  res.send('success')
})

// View Document
router.get('/view/:name', function (req, res) {
  const name = req.params.name
  const fileName = name + '.od'

  if (document.isPublic(fileName)) {
    // Compile the document
    compiler.compile(fileName)(req.app.get('sourceDirectory'))(
      req.app.get('destinationDirectory')
    )()

    const content = fs.readFileSync(
      path.join(req.app.get('destinationDirectory'), name + '.html')
    )

    res.set('Content-Type', 'text/html')
    res.end(content)
  } else {
    res.set('Content-Type', 'text/html')
    res.end('The page is private.')
  }
})

module.exports = router
