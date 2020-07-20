'use strict'

const xmlJs = require('xml-js')

exports.attributesFromString = function (attributesString) {
  return xmlJs.xml2js('<dummy ' + attributesString + '/>').elements[0]
    .attributes
}
