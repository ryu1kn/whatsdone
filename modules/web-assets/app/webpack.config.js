
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const FaviconsWebpackPlugin = require('favicons-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');
const webpack = require('webpack');

const SRC_DIR = path.resolve(__dirname, 'src');
const BUILD_DIR = path.resolve(__dirname, process.env.npm_package_config_buildDir);
const BYTE_LIMIT = 100000;

const devMode = process.env.NODE_ENV !== 'production'

module.exports = {
  entry: [
    'core-js/stable',
    'regenerator-runtime/runtime',
    'whatwg-fetch',
    `${SRC_DIR}/app.tsx`
  ],
  output: {
    path: `${BUILD_DIR}/static`,
    filename: 'build-[hash].js'
  },
  resolve: {
    extensions: ['.js', '.jsx', '.ts', '.d.ts', '.tsx', '.json'],
    fallback: {
      util: require.resolve('util')
    }
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        include: `${SRC_DIR}`,
        loader: 'babel-loader'
      },
      {
        test: /\.(less|css)$/,
        use: [
          devMode ? 'style-loader' : MiniCssExtractPlugin.loader,
          'css-loader',
          'less-loader',
        ]
      },
      {
        test: /\.(woff|woff2|ttf|svg)$/,
        type: 'asset',
        parser: {
          dataUrlCondition: {maxSize: BYTE_LIMIT}
        }
      },
      {
        test: /\.eot$/,
        type: 'asset/resource'
      }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
    }),
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
    new MiniCssExtractPlugin({filename: 'style-[hash].css'})
  ],
  devServer: {
    static: {
      directory: BUILD_DIR,
      watch: true
    },
    liveReload: true
  }
};
