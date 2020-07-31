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

exports.jsonToXml = jsonToXml
exports.format = format
exports.minify = minify
