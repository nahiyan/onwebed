const createError = require('http-errors')
const express = require('express')
const path = require('path')
const logger = require('morgan')
const { flash } = require('express-flash-message')
const session = require('express-session')
const http = require('http')

const indexRouter = require('./routes/index')
const bodyParser = require('body-parser')

module.exports.start = function (sourceDirectory) {
  return function (destinationDirectory) {
    return function (port) {
      var app = express()

      app.set('sourceDirectory', sourceDirectory)
      app.set('destinationDirectory', destinationDirectory)

      // Session
      app.set('trust proxy', 1) // trust first proxy
      app.use(
        session({
          secret: 'secret',
          key: 'onwebed',
          cookie: {
            httpOnly: true,
            secure: false,
            maxAge: null
          },
          saveUninitialized: false,
          resave: false
        })
      )

      // Flash messages
      app.use(flash())

      // Body parser
      app.use(bodyParser.urlencoded({ extended: true }))

      // view engine setup
      app.set('views', path.join(__dirname, 'views'))
      app.set('view engine', 'pug')

      app.use(logger('dev'))
      app.use(express.json())
      app.use(express.urlencoded({ extended: false }))
      app.use(express.static(path.join(__dirname, 'public')))
      app.use('/static', express.static(path.resolve(destinationDirectory)))

      // Routing
      app.use('/', indexRouter)

      // catch 404 and forward to error handler
      app.use(function (req, res, next) {
        next(createError(404))
      })

      // error handler
      app.use(function (err, req, res, next) {
        // set locals, only providing error in development
        res.locals.message = err.message
        res.locals.error = req.app.get('env') === 'development' ? err : {}

        // render the error page
        res.status(err.status || 500)
        res.render('base/error')
      })

      http.createServer(app).listen(port)
    }
  }
}
