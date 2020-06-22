#!/usr/bin/env node

const path = require('path')
const fs = require('fs')
const commandLineArgs = require('command-line-args')
const help = require('./help')
const compiler = require('./compiler')

// handle CLI arguments
const optionDefinitions = [
  { name: 'destination', alias: 'd', type: String },
  { name: 'help', alias: 'h', type: Boolean },
  {
    name: 'source',
    alias: 's',
    type: String,
    defaultOption: true
  }
]
try {
  const options = commandLineArgs(optionDefinitions)

  if (options.help === true) {
    // Help
    help.show()
  } else {
    // Process source directory
    const sourceDirectory = options.source === undefined ? '.' : options.source
    const outputDirectory =
      options.destination === undefined ? 'build' : options.destination

    // Create output directory if it doesn't exist
    try {
      fs.accessSync(outputDirectory)
    } catch (err) {
      fs.mkdirSync(outputDirectory)
    }

    fs.readdir(sourceDirectory, {}, (error, files) => {
      if (error === null) {
        files.forEach(fileName => {
          if (path.extname(fileName) === '.od') {
            compiler.compile(path.join(sourceDirectory, fileName), output => {
              fs.writeFile(
                path.join(outputDirectory, fileName + '.html'),
                output,
                'utf8',
                err => {
                  if (err) {
                    console.error(
                      "Error: Can't write to destination directory."
                    )
                  }
                }
              )
            })
          }
        })
      } else {
        console.error("Error: Can't read source directory.")
      }
    })
  }
} catch (e) {
  console.error(e)
}
