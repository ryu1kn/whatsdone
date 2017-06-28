
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
  }
};

gulp.task('build', ['compilecss', 'copyImages']);

gulp.task('compilecss', () =>
  gulp.src(paths.css.in)
    .pipe(less())
    .pipe(gulp.dest(paths.css.out))
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
