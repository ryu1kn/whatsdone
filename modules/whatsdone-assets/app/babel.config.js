module.exports = function config(api) {
  api.cache.invalidate(() => process.env.NODE_ENV === 'production')

  return {
    presets: [
      ['@babel/preset-env', {
        targets: {
          browsers: ['last 2 versions', 'safari >= 7']
        }
      }]
    ],

    overrides: [
      {
        test: /\.tsx?$/,
        presets: ['@babel/preset-typescript'],
      },
      {
        test: /\.tsx$/,
        presets: ['@babel/preset-react']
      }
    ]
  }
}
