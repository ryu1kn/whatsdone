
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const FaviconsWebpackPlugin = require('favicons-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');

const SRC_DIR = path.resolve(__dirname, 'src');
const BUILD_DIR = path.resolve(__dirname, process.env.npm_package_config_buildDir);
const BYTE_LIMIT = 8192;

const extractLess = new ExtractTextPlugin('style-[hash].css');
const jsMinifyPlugins = process.env.NODE_ENV === 'production' ? [
  new webpack.DefinePlugin({
    'process.env': {NODE_ENV: JSON.stringify('production')}
  }),
  new webpack.optimize.UglifyJsPlugin()
] : [];

module.exports = {
  entry: [
    'babel-polyfill',
    'whatwg-fetch',
    `${SRC_DIR}/app.js`
  ],
  output: {
    path: `${BUILD_DIR}/static`,
    filename: 'build-[hash].js'
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        include: `${SRC_DIR}`,
        loader: 'babel-loader'
      },
      {
        test: /\.(less|css)$/,
        use: extractLess.extract({
          use: ['css-loader', 'less-loader'],
          fallback: 'style-loader'
        })
      },
      {
        test: /\.(woff|woff2)$/,
        loader: 'url-loader',
        options: {
          limit: BYTE_LIMIT,
          mimetype: 'application/font-woff'
        }
      },
      {
        test: /\.ttf$/,
        loader: 'url-loader',
        options: {
          limit: BYTE_LIMIT,
          mimetype: 'application/octet-stream'
        }
      },
      {
        test: /\.eot$/,
        loader: 'file-loader'
      },
      {
        test: /\.svg$/,
        loader: 'url-loader',
        options: {
          limit: BYTE_LIMIT,
          mimetype: 'image/svg+xml'
        }
      }
    ]
  },
  plugins: [
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery'
    }),
    new FaviconsWebpackPlugin({
      logo: `${__dirname}/images/favicon.png`,
      prefix: 'static/icons-[hash]/'
    }),
    new HtmlWebpackPlugin({
      template: `${__dirname}/index.ejs`,
      filename: `${BUILD_DIR}/index.html`
    }),
    extractLess,
    ...jsMinifyPlugins
  ]
};
