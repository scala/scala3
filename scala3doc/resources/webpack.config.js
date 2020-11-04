const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const FileManagerPlugin = require('filemanager-webpack-plugin');

module.exports = {
  entry: './src/index.ts',
  devtool: 'inline-source-map',
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.s[ac]ss$/i,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
          'sass-loader',
        ],
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
  },
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist'),
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].css',
      chunkFilename: '[id].css',
    }),
    new FileManagerPlugin({
      events: {
        onEnd: {
          delete: ['./dotty_res/styles/main.css', './dotty_res/scripts/bundle.js'],
          move: [
            { source: 'dist/main.css', destination: './dotty_res/styles/main.css' },
            { source: 'dist/bundle.js', destination: './dotty_res/scripts/bundle.js' },
          ]
        }
      }
    })
  ],
};