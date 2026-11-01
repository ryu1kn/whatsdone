const assert = require('node:assert/strict');
const {chromium} = require('playwright');

(async () => {
  const browser = await chromium.launch();
  try {
    const context = await browser.newContext();
    const page = await context.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));

    if (process.env.ENV_NAME === 'ci') {
      await page.goto('https://whatsdone-ci.ryuichi.io/', {waitUntil: 'networkidle'});
      await page.fill('.modal-content.visible-md input[placeholder="Username"]', 'test-e2e');
      await page.fill('.modal-content.visible-md input[placeholder="Password"]', process.env.E2E_USER_PASSWORD);
      await page.click('.modal-content.visible-md input[value="Sign in"]');
    } else {
      const config = await page.request.get('http://localhost:8080/appConfig.json');
      assert.equal((await config.json()).API_ORIGIN, 'http://localhost:8081');
      await page.goto('http://localhost:8080/');
    }

    const testText = `test message @ ${new Date().toISOString()} + & café`;
    await page.getByLabel('Done Thing').fill(testText);
    const [created] = await Promise.all([
      page.waitForResponse(response => response.request().method() === 'POST' &&
        new URL(response.url()).pathname === '/dones'),
      page.getByRole('button', {name: 'Done!', exact: true}).click()
    ]);
    assert.ok(created.ok(), `Create DONE failed: ${created.status()}`);
    await page.reload();
    let item = page.locator('.doneitem').filter({hasText: testText});
    await item.waitFor();

    await item.dblclick();
    await item.locator('textarea').fill(`${testText} edited`);
    const [updated] = await Promise.all([
      page.waitForResponse(response => response.request().method() === 'PUT' &&
        new URL(response.url()).pathname.startsWith('/dones/')),
      item.getByRole('button', {name: 'Save', exact: true}).click()
    ]);
    assert.ok(updated.ok(), `Update DONE failed: ${updated.status()}`);
    await page.reload();
    item = page.locator('.doneitem').filter({hasText: `${testText} edited`});
    await item.waitFor();

    await item.hover();
    const [deleted] = await Promise.all([
      page.waitForResponse(response => response.request().method() === 'DELETE' &&
        new URL(response.url()).pathname.startsWith('/dones/')),
      item.getByRole('button', {name: 'Delete DONE'}).click()
    ]);
    assert.ok(deleted.ok(), `Delete DONE failed: ${deleted.status()}`);
    const loaded = page.waitForResponse(response => response.request().method() === 'GET' &&
      new URL(response.url()).pathname === '/dones');
    await page.reload();
    const reloaded = await loaded;
    assert.ok(reloaded.ok(), 'Reload DONEs failed');
    const body = await reloaded.json();
    assert.equal((body.items || body).some(done => done.doneThing === `${testText} edited`), false);
    await item.waitFor({state: 'hidden'});

    for (const width of [1280, 390]) {
      await page.setViewportSize({width, height: 900});
      await page.getByRole('button', {name: 'Done!', exact: true}).waitFor({state: 'visible'});
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false,
        `Horizontal overflow at ${width}px`);
    }
    assert.deepEqual(errors, [], 'Unexpected browser errors');
    console.log('PASS: load, create, edit, delete, desktop/mobile layout, no browser errors');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
