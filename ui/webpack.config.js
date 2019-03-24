var path = require('path');

module.exports = {
  mode: 'development',
  entry: './index.ts',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'app.bundle.js'
  },
  resolve: {
    extensions: ['.ts', '.js', '.elm', '.tsx', '.jsx']
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: 'elm-webpack-loader'
          }
          ]
      },
      {
        test: /\.ts$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: 'babel-loader'
          },
          {
            loader: 'ts-loader'
          }
        ]
      },
      {
        test: /\.js$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: 'babel-loader'
          }
        ]
      }
    ]
  }
};