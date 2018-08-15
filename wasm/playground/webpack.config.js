const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const path = require('path');

module.exports = {
  entry: "./src/playground.ts",
  // TODO: source maps are 10s of mb with monaco.
  // devtool: 'inline-source-map',
  output: {
    path: path.resolve(__dirname, "dist/js"),
    publicPath: 'js/',
    filename: "bundle.js",
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/
      },
      {
        test: /\.css$/,
        use: [ 'style-loader', 'css-loader' ]
      }
    ]
  },
  resolve: {
    extensions: [ '.tsx', '.ts', '.js', '.wasm', '.css' ]
  },
  mode: "development",
  // Workaround for https://github.com/webpack/webpack/issues/7760
  optimization: {
    usedExports: false
  },
  plugins: [
    new MonacoWebpackPlugin({
      languages: ['javascript'],
      features: ['bracketMatching', 'hover'],
    })
  ]
};
