const path = require('path')

const elmSrcPaths = [
  path.resolve(__dirname, 'assets/elm/document-editor'),
  path.resolve(__dirname, 'assets/elm/placeholders-editor')
]

const elmRules = elmSrcPaths.map(srcPath => ({
  test: new RegExp(`^${srcPath}.*elm$`),
  include: srcPath,
  exclude: [/elm-stuff/, /node_modules/],
  use: [
    {
      loader: require.resolve('elm-webpack-loader'),
      options: {
        cwd: srcPath
      }
    }
  ]
}))

module.exports = {
  entry: {
    index: './assets/js/index.js',
    document_editor: './assets/js/document-editor.js',
    placeholders_editor: './assets/js/placeholders-editor.js'
  },
  output: {
    path: path.resolve(__dirname, 'public/resources'),
    filename: '[name].js'
  },
  module: {
    rules: elmRules.concat([{
      test: /\.(sass)$/,
      use: [{
        // inject CSS to page
        loader: 'style-loader'
      }, {
        // translates CSS into CommonJS modules
        loader: 'css-loader'
      }, {
        // Run postcss actions
        loader: 'postcss-loader',
        options: {
          // `postcssOptions` is needed for postcss 8.x;
          // if you use postcss 7.x skip the key
          postcssOptions: {
            // postcss plugins, can be exported to postcss.config.js
            plugins: function () {
              return [
                require('autoprefixer')
              ]
            }
          }
        }
      }, {
        // compiles Sass to CSS
        loader: 'sass-loader'
      }]
    },
    {
      test: /\.css$/,
      use: [
        'style-loader',
        'css-loader'
      ]
    }, {
      test: /\.(eot|woff|woff2|svg|ttf)$/,
      use: ['file-loader']
    }])
  },
  resolve: {
    fallback: {
      buffer: require.resolve('buffer/')
    }
  }
}
