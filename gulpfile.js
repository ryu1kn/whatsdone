
var browserify = require('browserify'),
    source = require('vinyl-source-stream'),
    gulp  = require('gulp');

var paths = {
        js: {
            inEntry: './src/scripts/app.js',
            inAll  : './src/scripts/**/*.js',
            outName: 'app.js',
            outPath: './public/scripts'
        }
    };

gulp.task('watch', function() {
    gulp.watch(paths.js.inAll, ['compile']);
});

gulp.task('compile', function () {
    return browserify(paths.js.inEntry)
        .bundle()
        .pipe(source(paths.js.outName))
        .pipe(gulp.dest(paths.js.outPath));
});

gulp.task('default', ['compile', 'watch']);
