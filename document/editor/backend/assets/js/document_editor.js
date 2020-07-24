import './index.js'
import { Elm } from '../../../frontend/src/Main.elm'
import '../sass/document_editor.sass'
import { json2xml, xml2json } from 'xml-js'
import ace from 'ace-builds/src-noconflict/ace'
const pretty = require('pretty')

const content = document.getElementById('content').value
const name = document.getElementById('name').value

const app = Elm.Main.init({
  node: document.getElementById('documentBody'),
  flags: {
    content: content,
    fileName: name
  }
})

app.ports.documentToMarkup.subscribe(function (document) {
  app.ports.documentToMarkupResult.send(pretty(json2xml(document)))
})

app.ports.markupToDocument.subscribe(function (markup) {
  app.ports.markupToDocumentResult.send(xml2json(markup))
})

app.ports.setupMarkupEditor.subscribe(function (markup) {
  const editor = ace.edit('markup-editor', {
    maxLines: 1000
  })

  editor.session.setValue(markup)
  editor.session.setUseWrapMode(true)

  editor.on('change', function (e) {
    app.ports.updateMarkup.send(editor.getValue())
  })
})

app.ports.overlay.subscribe(function (enable) {
  if (enable === true) {
    document.querySelector('html').classList.add('no-scroll')
  } else {
    document.querySelector('html').classList.remove('no-scroll')
  }
})
