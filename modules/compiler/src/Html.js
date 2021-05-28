'use strict'

const xmlJs = require('xml-js')
const format = require('pretty')
const minify_ = require('html-minifier').minify
const path = require('path')
const fs = require('fs')

function jsonToXml (json) {
  return xmlJs.json2xml(json)
}

function minify (html) {
  return minify_(html, {
    keepClosingSlash: true,
    collapseWhitespace: true
  })
}

function processPlaceholders (sourceDirectory) {
  return function (html) {
    const placeholders = JSON.parse(fs.readFileSync(path.join(sourceDirectory, 'placeholders.json'), 'utf8'))

    const map = {}

    placeholders.forEach(function (placeholder) {
      map['{' + placeholder.key + '}'] = placeholder.value
    })

    const regex = new RegExp(Object.keys(map).join('|'), 'gi')

    return html.replace(regex, function (match) {
      return map[match]
    })
  }
}

exports.jsonToXml = jsonToXml
exports.format = format
exports.minify = minify
exports.processPlaceholders = processPlaceholders
