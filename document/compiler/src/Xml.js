'use strict'

const xmlJs = require('xml-js')

exports.attributesFromString = function (attributesString) {
  if (attributesString.trim().length > 0) {
    return xmlJs.xml2js('<dummy ' + attributesString + '/>').elements[0]
      .attributes
  } else {
    return {}
  }
}

exports.toJson = function (xml) {
  return xmlJs.xml2json(xml)
}
