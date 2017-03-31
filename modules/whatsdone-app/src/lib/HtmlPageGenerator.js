
'use strict';

const ServiceLocator = require('./ServiceLocator');
const path = require('path');

class HtmlPageGenerator {

  constructor() {
    this._pug = ServiceLocator.pug;

    this._cache = {};
  }

  generate(pageName, variables) {
    const cachedTemplate = this._cache[pageName];
    const template = cachedTemplate || this._loadTemplate(pageName);
    return template(variables);
  }

  _loadTemplate(pageName) {
    const templateLocation = path.join(__dirname, 'views', `${pageName}.pug`);
    const template = this._pug.compileFile(templateLocation);
    this._cache[pageName] = template;
    return template;
  }

}

module.exports = HtmlPageGenerator;
