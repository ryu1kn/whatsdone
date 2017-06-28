
const gulp = require('gulp');
const less = require('gulp-less');

const SRC_DIR = './src';
const DIST_DIR = './dist';

const paths = {
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

gulp.task('build', ['compilecss', 'copyHtml', 'copyImages']);

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
  gulp.watch(paths.css.in, ['compilecss']);
  gulp.watch(paths.images.in, ['copyImages']);
});

gulp.task('default', ['compilecss', 'watch']);
