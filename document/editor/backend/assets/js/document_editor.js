import './index.js'
import { Elm } from '../../../frontend/src/Main.elm'
import '../sass/document_editor.sass'
import '@fortawesome/fontawesome-free/js/all'

const content = document.getElementById('content').value
const name = document.getElementById('name').value

Elm.Main.init({
  node: document.getElementById('documentBody'),
  flags: {
    content: content,
    fileName: name
  }
})
