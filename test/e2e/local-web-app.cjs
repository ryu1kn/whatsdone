const assert = require('node:assert/strict');
const {chromium} = require('playwright');

// Uses the local mock API only. Never writes to a deployed environment.
(async () => {
  const browser = await chromium.launch({channel: 'chrome'});
  try {
    const page = await browser.newPage({viewport: {width: 1280, height: 900}});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    const config = await page.request.get('http://localhost:8080/appConfig.json');
    assert.equal((await config.json()).API_ORIGIN, 'http://localhost:8081');
    await page.goto('http://localhost:8080/');
    await page.getByText('This is my first done!', {exact: true}).waitFor();
    const text = `Dependency upgrade ${Date.now()} + & café`;
    await page.getByLabel('Done Thing').fill(text);
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' && response.status() === 202),
      page.getByRole('button', {name: 'Done!', exact: true}).click()
    ]);
    await page.reload();
    let item = page.locator('.doneitem').filter({hasText: text});
    await item.waitFor();
    await item.dblclick();
    await item.locator('textarea').fill(`${text} edited`);
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'PUT' && response.status() === 202),
      item.getByRole('button', {name: 'Save', exact: true}).click()
    ]);
    await page.reload();
    item = page.locator('.doneitem').filter({hasText: `${text} edited`});
    await item.waitFor();
    await item.hover();
    await Promise.all([
      page.waitForResponse(response => response.request().method() === 'DELETE' && response.status() === 204),
      item.getByRole('button', {name: 'Delete DONE'}).click()
    ]);
    await page.reload();
    await page.getByText('This is my first done!', {exact: true}).waitFor();
    assert.equal(await page.getByText(`${text} edited`, {exact: true}).count(), 0);
    for (const width of [1280, 390]) {
      await page.setViewportSize({width, height: 900});
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false);
      await page.getByRole('button', {name: 'Done!', exact: true}).waitFor({state: 'visible'});
      await page.screenshot({path: `/private/tmp/whatsdone-upgraded-${width}.png`, fullPage: true});
    }
    assert.deepEqual(errors, []);
    console.log('PASS: load, create, edit, delete, desktop/mobile layout, no browser errors');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
