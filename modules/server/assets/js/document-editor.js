import './index.js'
import { Elm } from '../elm/document-editor/src/Main.elm'
import '../sass/document_editor.sass'
import { json2xml, xml2json } from 'xml-js'
import ace from 'ace-builds/src-noconflict/ace'
const pretty = require('pretty')

global.Buffer = global.Buffer || require('buffer').Buffer

const content = document.getElementById('content').value
const name = document.getElementById('name').value

const app = Elm.Main.init({
  node: document.getElementById('document-body'),
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

const expandTextarea = function (element) {
  const scrollX = window.scrollX
  const scrollY = window.scrollY

  // Reset element height
  element.style.height = 'inherit'

  // Get the computed styles for the element
  const computed = window.getComputedStyle(element)
  const height =
    parseInt(computed.getPropertyValue('border-top-width'), 10) +
    parseInt(computed.getPropertyValue('padding-top'), 10) +
    element.scrollHeight +
    parseInt(computed.getPropertyValue('padding-bottom'), 10) +
    parseInt(computed.getPropertyValue('border-bottom-width'), 10)

  element.style.height = height + 'px'

  window.scrollTo(scrollX, scrollY)
}

app.ports.expandTextarea.subscribe(function (ids) {
  ids.forEach(function (id) {
    expandTextarea(document.querySelector('#element' + id + ' > textarea'))
  })
})
