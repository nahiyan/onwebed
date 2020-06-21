#!/usr/bin/env node

const path = require('path')
const fs = require('fs')
const commandLineArgs = require('command-line-args')
const help = require('./help')
const compiler = require('./compiler')

// handle CLI arguments
const optionDefinitions = [
  { name: 'output', alias: 'o', type: String },
  { name: 'help', alias: 'h', type: Boolean },
  {
    name: 'directory',
    alias: 'd',
    type: String,
    defaultOption: true
  }
]
try {
  const options = commandLineArgs(optionDefinitions)

  // Help
  if (options.help === true) {
    help.show()
  } else {
    const sourceDirectory =
      options.directory === undefined ? '.' : options.directory

    fs.readdir(sourceDirectory, {}, (error, files) => {
      if (error === null) {
        files.forEach(fileName => {
          if (path.extname(fileName) === '.od') {
            compiler.compile(path.join(sourceDirectory, fileName))
          }
        })
      } else {
        console.log("Error: Can't read source directory.")
      }
    })
  }
} catch (e) {
  console.log(e)
}
