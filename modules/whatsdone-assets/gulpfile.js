
const browserify = require('browserify');
const source = require('vinyl-source-stream');
const gulp = require('gulp');
const less = require('gulp-less');

const SRC_DIR = './src';
const DIST_DIR = './dist';

const paths = {
  js: {
    inEntry: SRC_DIR + '/client/scripts/app.js',
    inAll: SRC_DIR + '/client/scripts/**/*.js',
    outName: 'app.js',
    outPath: DIST_DIR + '/scripts'
  },
  css: {
    in: SRC_DIR + '/stylesheets/**/*.less',
    out: DIST_DIR + '/css'
  },
  images: {
    in: SRC_DIR + '/images/**/*',
    out: DIST_DIR + '/images'
  },
  html: {
    in: SRC_DIR + '/index.html',
    out: DIST_DIR
  }
};

gulp.task('build', ['compile', 'copyHtml', 'copyImages']);

gulp.task('compile', ['compilejs', 'compilecss']);

gulp.task('compilejs', () =>
  browserify(paths.js.inEntry).bundle()
    .pipe(source(paths.js.outName))
    .pipe(gulp.dest(paths.js.outPath))
);

gulp.task('compilecss', () =>
  gulp.src(paths.css.in)
    .pipe(less())
    .pipe(gulp.dest(paths.css.out))
);

gulp.task('copyHtml', () =>
  gulp.src(paths.html.in)
    .pipe(gulp.dest(paths.html.out))
);

gulp.task('copyImages', () =>
  gulp.src(paths.images.in)
    .pipe(gulp.dest(paths.images.out))
);

gulp.task('watch', () => {
  gulp.watch(paths.js.inAll, ['compilejs']);
  gulp.watch(paths.css.in, ['compilecss']);
  gulp.watch(paths.images.in, ['copyImages']);
});

gulp.task('default', ['compilejs', 'compilecss', 'watch']);
