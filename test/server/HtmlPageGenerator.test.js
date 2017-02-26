
const HtmlPageGenerator = require('../../src/server/HtmlPageGenerator');
const ServiceLocator = require('../../src/server/ServiceLocator');

describe('Server HtmlPageGenerator', () => {

  it('generates a html page with pug library', () => {
    const compiledTemplate = sinon.stub().returns('HTML');
    const fakePug = {compileFile: sinon.stub().returns(compiledTemplate)};
    ServiceLocator.load({createPug: () => fakePug});
    const htmlPageGenerator = new HtmlPageGenerator();

    expect(htmlPageGenerator.generate('PAGE_NAME', 'PAGE_VARIABLE')).to.eql('HTML');
    expect(fakePug.compileFile.args[0][0]).to.have.string('/views/PAGE_NAME.pug');
    expect(compiledTemplate).to.have.been.calledWith('PAGE_VARIABLE');
  });

  it('uses a cached compiled template if it exists', () => {
    const compiledTemplate = sinon.stub().returns('HTML');
    const fakePug = {compileFile: sinon.stub().returns(compiledTemplate)};
    ServiceLocator.load({createPug: () => fakePug});
    const htmlPageGenerator = new HtmlPageGenerator();

    htmlPageGenerator.generate('PAGE_NAME', 'PAGE_VARIABLE_1');
    htmlPageGenerator.generate('PAGE_NAME', 'PAGE_VARIABLE_2');

    expect(fakePug.compileFile).to.have.been.calledOnce;
    expect(compiledTemplate.args).to.eql([
      ['PAGE_VARIABLE_1'], ['PAGE_VARIABLE_2']
    ]);
  });

});
