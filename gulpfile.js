var gulp = require('gulp');
var elm  = require('gulp-elm');
var del = require('del');
var uglify = require('gulp-uglify');
var spawn = require('cross-spawn');

// develop with elm-live
gulp.task('elm-live', function() {
  spawn.sync('elm-live',
    ['src/Pilots.elm', '--debug', '--output', 'elm.js', '--open'],
    { stdio: 'inherit' } );
});

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  return gulp.src('src/*.elm')
    .pipe(elm.bundle('elm.js'))
    .pipe(uglify())
    .pipe(gulp.dest('dist/'));
});

gulp.task('clean', function () {
  return del(['dist/']);
});

gulp.task('copy-html', function() {
  gulp.src('index.html')
  .pipe(gulp.dest('dist'));
});

gulp.task('default', ['elm-live']);

gulp.task('dist', ['clean', 'elm', 'copy-html']);
