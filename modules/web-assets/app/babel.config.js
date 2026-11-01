module.exports = function config(api) {
  api.cache.invalidate(() => process.env.NODE_ENV === 'production')

  return {
    targets: {browsers: ['last 2 versions', 'safari >= 7']},
    presets: [['@babel/preset-env', {modules: api.env('test') ? 'commonjs' : false}]],

    overrides: [
      {
        test: /\.tsx?$/,
        presets: ['@babel/preset-typescript'],
      },
      {
        test: /\.tsx$/,
        presets: [['@babel/preset-react', {runtime: 'automatic'}]]
      }
    ]
  }
}
