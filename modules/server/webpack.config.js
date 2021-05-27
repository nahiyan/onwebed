const path = require('path')

module.exports = {
  entry: {
    index: './assets/js/index.js',
    document_editor: './assets/js/document-editor.js'
  },
  output: {
    path: path.resolve(__dirname, 'public/resources'),
    filename: '[name].js'
  },
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      loader: 'elm-webpack-loader',
      options: { cwd: path.resolve(__dirname, 'assets/elm/document-editor') }
    }, {
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
    }]
  },
  resolve: {
    fallback: {
      buffer: require.resolve('buffer/')
    }
  }
}
