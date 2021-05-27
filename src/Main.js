'use strict'

const compiler = require('../modules/build/compiler')
const app = require('../modules/server/app')

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
