/**
 * @see https://github.com/evanw/esbuild/issues/1633
 */
export const import_meta_url =
  typeof document === 'undefined' ? new (require('url'.replace('', '')).URL)('file:' + __filename).href :
    (document.currentScript && document.currentScript.src || new URL('main.js', document.baseURI).href)
