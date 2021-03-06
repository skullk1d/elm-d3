var path = require('path');
var webpack = require('webpack');
var merge = require('webpack-merge');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var autoprefixer = require('autoprefixer');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');


const prod = 'production';
const dev = 'development';

// determine build env
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;
const isDev = TARGET_ENV === dev;
const isProd = TARGET_ENV === prod;

// entry and output path/filename variables
const entryPath = path.join(__dirname, 'src/index.ts');
const outputPath = path.join(__dirname, 'dist');
const outputFilename = isProd ? '[name]-[hash].js' : '[name].js'

console.log('WEBPACK Building for ' + TARGET_ENV);

// common webpack config (valid for dev and prod)
var commonConfig = {
    output: {
        path: outputPath,
        filename: `assets/js/${outputFilename}`,
    },
    resolve: {
        extensions: ['.js', '.elm', '.ts', '.tsx'],
        modules: ['node_modules']
    },
    module: {
        noParse: /\.elm$/,
        rules: [
            {
                test: /\.(eot|ttf|woff|woff2|svg)$/,
                use: 'file-loader?publicPath=../../&name=assets/css/[hash].[ext]'
            },
            { 
                test: /\.tsx?$/, 
                loader: 'ts-loader' 
            }
        ]
    },
    plugins: [
        new webpack.LoaderOptionsPlugin({
            options: {
                postcss: [autoprefixer()]
            }
        }),
        new HtmlWebpackPlugin({
            template: 'src/index.html',
            inject: 'body',
            filename: 'index.html'
        })
    ]
}

// additional webpack settings for local env (when invoked by 'npm start')
if (isDev) {
    module.exports = merge(commonConfig, {
        entry: entryPath,
        devServer: {
            // serve index.html in place of 404 responses
            historyApiFallback: true,
            contentBase: './src',
            hot: true
        },
        module: {
            rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [{
                    loader: 'elm-webpack-loader',
                    options: {
                        verbose: true,
                        warn: true,
                        debug: true
                    }
                }]
            },{
                test: /\.sc?ss$/,
                use: ['style-loader', 'css-loader', 'postcss-loader', 'sass-loader']
            }]
        },
        devtool: 'inline-source-map'
    });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (isProd) {
    module.exports = merge(commonConfig, {
        entry: entryPath,
        module: {
            rules: [{
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: 'elm-webpack-loader'
            }, {
                test: /\.sc?ss$/,
                use: ExtractTextPlugin.extract({
                    fallback: 'style-loader',
                    use: ['css-loader', 'postcss-loader', 'sass-loader']
                })
            }]
        },
        plugins: [
            new ExtractTextPlugin({
                filename: 'assets/css/[name]-[hash].css',
                allChunks: true,
            }),
            new CopyWebpackPlugin([{
                from: 'src/assets/img/',
                to: 'assets/img/'
            }]),
            new CopyWebpackPlugin([{
                from: 'src/assets/json/',
                to: 'assets/json/'
            }]),

            // extract CSS into a separate file
            // minify & mangle JS/CSS
            new webpack.optimize.UglifyJsPlugin({
                minimize: true,
                compressor: {
                    warnings: false
                }
                // mangle:  true
            })
        ],
        devtool: 'source-map'
    });
}
