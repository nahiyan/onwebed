const express = require('express')
const router = express.Router()
const document = require('../../../../lib/document/utilities')
const compiler = require('../../../../lib/document/compiler')
const path = require('path')
const xmlJs = require('xml-js')
const fs = require('fs')
const pretty = require('pretty')
const snakeCase = require('snake-case')
const minify = require('html-minifier').minify

// Homepage
router.get('/', async function (req, res, next) {
  const sourceDirectory = req.app.get('sourceDirectory')

  const documents = fs
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

      let name = ''
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

router.get('/delete/:name', async function (req, res, next) {
  const name = req.params.name
  const documentFileName = path.join(
    req.app.get('sourceDirectory'),
    name + '.od'
  )

  if (fs.existsSync(documentFileName)) {
    fs.unlinkSync(documentFileName)
    await req.flash('success', `Document ${name} deleted successfully!`)
    res.redirect('/')
  } else {
    await req.flash('danger', `Document ${name} doesn't exist.`)
    res.redirect('/')
  }
})

// Edit Document
router.get('/edit/:name', function (req, res) {
  const name = req.params.name
  const content = xmlJs.xml2js(
    minify(
      fs.readFileSync(
        path.join(req.app.get('sourceDirectory'), name + '.od'),
        'utf8'
      ),
      {
        keepClosingSlash: true,
        collapseWhitespace: true
      }
    )
  )
  res.render('documents/edit', {
    content: content,
    name: name
  })
})

// Save Document
router.post('/save/:name', async function (req, res) {
  const name = req.params.name
  const fileName = name + '.od'
  const document = req.body
  const markup = pretty(xmlJs.js2xml(document))

  // Write the file
  fs.writeFileSync(path.join(req.app.get('sourceDirectory'), fileName), markup)

  // Compile all documents, just in case they got dependencies
  try {
    await compiler.compileFromDirectory(req.app.get('sourceDirectory'))(
      req.app.get('destinationDirectory')
    )()

    res.send('success')
  } catch (e) {
    res.send('Error: Failed to compile documents.')
  }
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

    res.set('Content-Type', 'text/html')
    res.redirect(path.join('/static', name + '.html'))
  } else {
    res.set('Content-Type', 'text/html')
    res.end('The page is private.')
  }
})

// View placeholders
router.get('/placeholders', async function (req, res, next) {
  const sourceDirectory = req.app.get('sourceDirectory')
  const placeholdersFile = path.join(sourceDirectory, 'placeholders.json')

  let placeholders = []
  if (fs.existsSync(placeholdersFile)) {
    placeholders = JSON.parse(fs.readFileSync(placeholdersFile))
  }

  res.render('placeholders/index', {
    title: 'Onwebed - Placeholders',
    placeholders: placeholders,
    sourceDirectory: req.app.get('sourceDirectory'),
    messagesDanger: await req.consumeFlash('danger'),
    messagesSuccess: await req.consumeFlash('success')
  })
})

module.exports = router
