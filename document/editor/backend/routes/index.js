const express = require('express')
const router = express.Router()
const document = require('../../../../lib/document/utilities')
const compiler = require('../../../../lib/document/compiler')
const path = require('path')
const xmlJs = require('xml-js')
const fs = require('fs')
const pretty = require('pretty')
const snakeCase = require('snake-case')

/* GET home page. */
router.get('/', async function (req, res, next) {
  const sourceDirectory = req.app.get('sourceDirectory')

  var documents = fs
    .readdirSync(sourceDirectory, 'utf8')
    .filter(document.isDocument)
    .map(function (file) {
      return path.basename(file, path.extname(file))
    })
    .map(function (machineName) {
      const elements = xmlJs.xml2js(
        fs.readFileSync(path.join(sourceDirectory, machineName + '.od'), {
          encoding: 'utf8'
        }),
        { compact: true }
      )

      var name = ''
      if (
        elements.document !== undefined &&
        elements.document.head !== undefined &&
        elements.document.head.name !== undefined
      ) {
        name = elements.document.head.name._text
      }

      return { machineName: machineName, name: name }
    })

  res.render('documents/index', {
    title: 'Onwebed - Documents',
    documents: documents,
    sourceDirectory: req.app.get('sourceDirectory'),
    destinationDirectory: req.app.get('destinationDirectory'),
    messagesDanger: await req.consumeFlash('danger'),
    messagesSuccess: await req.consumeFlash('success')
  })
})

router.get('/new', function (req, res, next) {
  res.render('documents/new', {
    title: 'Onwebed - Create New Document'
  })
})
router.post('/new', async function (req, res, next) {
  const name = req.body.name
  const sourceDirectory = req.app.get('sourceDirectory')

  if (name !== undefined && name.trim().length > 0) {
    fs.writeFileSync(
      path.join(sourceDirectory, snakeCase.snakeCase(name) + '.od'),
      pretty(
        `<document><head><name>${name}</name></head><body></body></document>`
      )
    )

    await req.flash('success', `Document ${name} created successfully!`)
    res.redirect('/')
  } else {
    await req.flash('danger', "Name can't be blank!")
    res.render('documents/new', {
      title: 'Onwebed - Create New Document',
      messagesDanger: await req.consumeFlash('danger')
    })
  }
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
