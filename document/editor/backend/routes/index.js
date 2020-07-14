const express = require('express')
const router = express.Router()
const document = require('../../../../lib/document/utilities')
const path = require('path')
const xmlJs = require('xml-js')
const fs = require('fs')
const pretty = require('pretty')

/* GET home page. */
router.get('/', function (req, res, next) {
  const documentNames = fs
    .readdirSync(req.app.get('sourceDirectory'), 'utf8')
    .filter(document.isPublic)
    .map(function (file) {
      return path.basename(file, path.extname(file))
    })
  res.render('documents/index', {
    title: 'Onwebed - Documents',
    documentNames: documentNames
  })
})

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
  const document = req.body
  const markup = pretty(xmlJs.js2xml(document))
  fs.writeFileSync(
    path.join(req.app.get('sourceDirectory'), name + '.od'),
    markup
  )

  res.send('success')
})

// View Document
router.get('/view/:name', function (req, res) {
  const name = req.params.name
  const content = fs.readFileSync(
    path.join(req.app.get('destinationDirectory'), name + '.od' + '.html')
  )

  res.set('Content-Type', 'text/html')
  res.end(content)
})

module.exports = router
