const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const path = require('path');

module.exports = {
  entry: "./src/playground.ts",
  // source maps create massive outputs with monaco.
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
  plugins: [
    new MonacoWebpackPlugin({
      languages: ['javascript'],
      features: ['bracketMatching', 'hover'],
    })
  ]
};
