
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
    `${SRC_DIR}/app.js`
  ],
  output: {
    path: `${BUILD_DIR}/static`,
    filename: 'build.js'
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: `${SRC_DIR}`,
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
      template: `${__dirname}/index.ejs`,
      filename: `${BUILD_DIR}/index.html`
    }),
    new FaviconsWebpackPlugin({
      logo: `${__dirname}/images/favicon.png`,
      prefix: 'static/icons-[hash]/'
    }),
    extractLess,
    ...jsMinifyPlugins
  ]
};
