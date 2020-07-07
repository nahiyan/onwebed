import './index.js'
import { Elm } from '../../../frontend/src/Main.elm'
import '../sass/document_editor.sass'
import '@fortawesome/fontawesome-free/js/all'

const pageContent = document.getElementById('page-content').value
const pageName = document.getElementById('page-name').value

Elm.Main.init({
  node: document.getElementById('content'),
  flags: {
    content: pageContent,
    pageName: pageName
  }
})
