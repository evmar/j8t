const path = require('path');

module.exports = {
  entry: "./src/playground.ts",
  devtool: 'inline-source-map',
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
      }
    ]
  },
  resolve: {
    extensions: [ '.tsx', '.ts', '.js', '.wasm' ]
  },
  mode: "development"
};

