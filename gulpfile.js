// modules
var gulp = require('gulp');
var plumber = require('gulp-plumber');
var sourcemaps = require('gulp-sourcemaps');
var sass = require('gulp-sass');
var prefixer = require('gulp-autoprefixer');
var minify = require('gulp-minify-css');
var notifier = require('node-notifier');
var util = require('gulp-util');

function error(err) {
  notifier.notify({message: 'Error: ' + err.message});
  util.log(util.colors.red('Error: ' + err.message));
}

function sassDev() {
  return gulp.src('./sass/main.scss')
    .pipe(plumber({errorHandler: error}))
    .pipe(sourcemaps.init())
    .pipe(sass())
    .pipe(prefixer())
    .pipe(sourcemaps.write('./maps'))
    .pipe(gulp.dest('./assets/css'));
}

function sassProd() {
  return gulp.src('./sass/main.scss')
    .pipe(plumber({errorHandler: error}))
    .pipe(sass())
    .pipe(prefixer())
    .pipe(minify())
    .pipe(gulp.dest('./assets/css'));
}

gulp.task('dev', sassDev);
gulp.task('prod', sassProd);
gulp.task('watch', function() {
  gulp.watch('./sass/**/*.scss', ['dev']);
});
