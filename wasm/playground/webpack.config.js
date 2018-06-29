const path = require('path');

module.exports = {
  entry: "./src/playground.js",
  output: {
    path: path.resolve(__dirname, "dist/js"),
    publicPath: 'js/',
    filename: "bundle.js",
  },
  mode: "development"
};

