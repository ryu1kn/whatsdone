
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const FaviconsWebpackPlugin = require('favicons-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');

const SRC_DIR = path.resolve(__dirname, 'src');
const BUILD_DIR = path.resolve(__dirname, process.env.npm_package_config_buildDir);

const extractLess = new ExtractTextPlugin('style.css');
const jsMinifyPlugins = process.env.NODE_ENV === 'production' ? [
  new webpack.DefinePlugin({
    'process.env': {NODE_ENV: JSON.stringify('production')}
  }),
  new webpack.optimize.UglifyJsPlugin()
] : [];

module.exports = {
  entry: [
    'whatwg-fetch',
    `${SRC_DIR}/client/scripts/app.js`
  ],
  output: {
    path: BUILD_DIR,
    filename: 'build.js'
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: `${SRC_DIR}/client`,
        loader: 'babel-loader'
      },
      {
        test: /\.less$/,
        use: extractLess.extract({
          use: ['css-loader', 'less-loader'],
          fallback: 'style-loader'
        })
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: `${SRC_DIR}/index.ejs`,
      filename: `${BUILD_DIR}/index.html`
    }),
    new FaviconsWebpackPlugin(`${SRC_DIR}/images/favicon.png`),
    extractLess,
    ...jsMinifyPlugins
  ]
};