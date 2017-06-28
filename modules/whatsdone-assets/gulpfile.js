
const gulp = require('gulp');

const SRC_DIR = './src';
const DIST_DIR = './dist';

const paths = {
  images: {
    in: SRC_DIR + '/images/**/*',
    out: DIST_DIR + '/images'
  }
};

gulp.task('build', ['copyImages']);

gulp.task('copyImages', () =>
  gulp.src(paths.images.in)
    .pipe(gulp.dest(paths.images.out))
);

gulp.task('watch', () => {
  gulp.watch(paths.images.in, ['copyImages']);
});

gulp.task('default', ['copyImages', 'watch']);
