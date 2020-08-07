'use strict'

const compiler = require('../lib/document/compiler')
const app = require('../document/editor/backend/app')

function startServer (sourceDirectory) {
  return function (destinationDirectory) {
    return function (port) {
      app.start(sourceDirectory)(destinationDirectory)(port)
      return true
    }
  }
}

exports.startServer = startServer
exports.compileFromDirectory = compiler.compileFromDirectory
