
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
  },
  images: {
    in: './src/images/**/*',
    out: './public/images'
  }
};

gulp.task('build', ['compile', 'copyImages']);

gulp.task('compile', ['compilejs', 'compilecss']);

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

gulp.task('copyImages', () =>
  gulp.src(paths.images.in)
    .pipe(gulp.dest(paths.images.out))
);

gulp.task('watch', function () {
  gulp.watch(paths.js.inAll, ['compilejs']);
  gulp.watch(paths.css.in, ['compilecss']);
  gulp.watch(paths.images.in, ['copyImages']);
});

gulp.task('default', ['compilejs', 'compilecss', 'watch']);
