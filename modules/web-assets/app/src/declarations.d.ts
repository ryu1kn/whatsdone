// Ambient module declarations so `tsc` can type-check the project.
//
// - `*.css` / `*.less` are side-effect imports resolved at build time by the
//   webpack css/less loaders; they have no types of their own.
// - `querystring` / `url` resolve to untyped browser polyfill packages pulled
//   in for the webpack bundle.
declare module '*.css';
declare module '*.less';
declare module 'querystring';
declare module 'url';
