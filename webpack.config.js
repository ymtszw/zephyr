const BASE = `${__dirname}/dist/`
const IS_CLOUD = (process.env.WEBPACK_BUILD_ENV === 'cloud')

module.exports = {
  entry: {
    blick: `${__dirname}/src/zephyr.js`
  },
  output: {
    path: BASE,
    filename: 'index.js',
    libraryTarget: 'window',
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        loader: ['style-loader', 'css-loader'],
      },
      {
        test: /\.elm$/,
        loader: 'elm-webpack-loader',
        options: {
          warn: true,
          debug: !IS_CLOUD,
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
        options: {
          presets: [
            ['env', { targets: { browsers: ['since 2015'] } }]
          ]
        }
      }
    ]
  },
  mode: IS_CLOUD ? 'production' : 'development',
  serve: {
    contentBase: BASE,
    port: '8079',
  }
}
