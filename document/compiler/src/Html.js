'use strict'

const xmlJs = require('xml-js')
const format = require('pretty')
const minify_ = require('html-minifier').minify

function jsonToXml (json) {
  return xmlJs.json2xml(json)
}

function minify (html) {
  return minify_(html, {
    keepClosingSlash: true,
    collapseWhitespace: true
  })
}

function processSpecialText (html) {
  return html
    .replace(/{space}/g, '&nbsp;')
    .replace(/{tab}/g, '&#9;')
    .replace(/{new-line}/g, '<br/>')
}

exports.jsonToXml = jsonToXml
exports.format = format
exports.minify = minify
exports.processSpecialText = processSpecialText
