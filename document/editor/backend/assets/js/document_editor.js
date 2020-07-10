import './index.js'
import { Elm } from '../../../frontend/src/Main.elm'
import '../sass/document_editor.sass'
import '@fortawesome/fontawesome-free/js/all'
import { json2xml } from 'xml-js'
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

app.ports.documentToXml.subscribe(function (documentBody) {
  app.ports.documentToXmlResult.send(pretty(json2xml(documentBody)))
})

app.ports.overlay.subscribe(function (enable) {
  if (enable === true) {
    document.querySelector('html').classList.add('no-scroll')
  } else {
    document.querySelector('html').classList.remove('no-scroll')
  }
})
