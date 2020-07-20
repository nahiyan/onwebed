'use strict'

const xmlJs = require('xml-js')
const format = require('pretty')

function jsonToXml (json) {
  return xmlJs.json2xml(json)
}

exports.jsonToXml = jsonToXml
exports.format = format
