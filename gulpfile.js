
const browserify = require('browserify');
const source = require('vinyl-source-stream');
const gulp = require('gulp');

var less = require('gulp-less');

var paths = {
  js: {
    inEntry: './src/client/scripts/app.js',
    inAll: './src/client/scripts/**/*.js',
    outName: 'app.js',
    outPath: './public/scripts'
  },
  css: {
    in: './src/stylesheets/**/*.less',
    out: './public/css'
  }
};

gulp.task('compile', ['compilejs', 'compilecss']);

gulp.task('watch', function () {
  gulp.watch(paths.js.inAll, ['compilejs']);
  gulp.watch(paths.css.in, ['compilecss']);
});

gulp.task('compilejs', function () {
  return browserify(paths.js.inEntry)
      .bundle()
      .pipe(source(paths.js.outName))
      .pipe(gulp.dest(paths.js.outPath));
});

gulp.task('compilecss', function () {
  return gulp.src(paths.css.in)
    .pipe(less())
    .pipe(gulp.dest(paths.css.out));
});

gulp.task('default', ['compilejs', 'compilecss', 'watch']);
