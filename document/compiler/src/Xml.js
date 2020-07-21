'use strict'

const xmlJs = require('xml-js')

exports.attributesFromString = function (attributesString) {
  return xmlJs.xml2js('<dummy ' + attributesString + '/>').elements[0]
    .attributes
}

exports.toJson = function (xml) {
  return xmlJs.xml2json(xml)
}
