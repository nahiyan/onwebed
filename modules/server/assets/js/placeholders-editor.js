import './index.js'
import { Elm } from '../elm/placeholders-editor/src/Main.elm'

global.Buffer = global.Buffer || require('buffer').Buffer

const content = document.getElementById('content').value

Elm.Main.init({
  node: document.getElementById('app'),
  flags: JSON.parse(content)
})
