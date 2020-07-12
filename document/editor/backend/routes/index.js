const express = require('express')
const router = express.Router()
const document = require('../../../../lib/document/base')
const path = require('path')
const xmlJs = require('xml-js')
const fs = require('fs')
const pretty = require('pretty')

const sourceDirectory = path.resolve('../../../site')

/* GET home page. */
router.get('/', function (req, res, next) {
  const documents = document
    .sourceItems(sourceDirectory)
    .filter(function (file) {
      return document.isPublic(file)
    })
    .map(function (file) {
      return path.basename(file, path.extname(file))
    })
  res.render('documents/index', {
    title: 'Onwebed - Documents',
    documents: documents
  })
})

// Edit Document
router.get('/edit/:name', function (req, res) {
  const name = req.params.name
  const content = xmlJs.xml2js(
    document.content(path.join(sourceDirectory, name + '.od'))
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
  fs.writeFileSync(path.join(sourceDirectory, name + '.od'), markup)

  res.send('success')
})

module.exports = router
