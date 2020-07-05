const prettify = require('pretty')

function format (html) {
  return prettify(html)
}

export { format }
