var express = require('express')
var router = express.Router()
const document = require('../../../../lib/document/base')
const path = require('path')

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
  res.render('pages/index', { title: 'Onwebed - Pages', documents: documents })
})

router.get('/edit/:name', function (req, res) {
  const name = req.params.name
  res.render('pages/edit', { title: 'Onwebed - Pages' })
})

module.exports = router
