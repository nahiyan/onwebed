'use strict'

const app = require('../document/editor/backend/app')
const http = require('http')

function startServer (sourceDirectory) {
  return function (destinationDirectory) {
    return function (port) {
      const server = http.createServer(app)
      server.listen(port)
      return true
    }
  }
}

exports.startServer = startServer
