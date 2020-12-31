const { chromium } = require('playwright');

// The code is originally generated by https://www.npmjs.com/package/playwright-cli
(async () => {
  const browser = await chromium.launch();
  const context = await browser.newContext();

  const page = await context.newPage();

  await page.goto('https://whatsdone-ci.ryuichi.io/', {waitUntil: 'networkidle'});

  await page.click('(//input[normalize-space(@placeholder)=\'Username\' and normalize-space(@type)=\'text\' and normalize-space(@name)=\'username\'])[2]');
  await page.fill('(//input[normalize-space(@placeholder)=\'Username\' and normalize-space(@type)=\'text\' and normalize-space(@name)=\'username\'])[2]', 'test-e2e');

  await page.press('(//input[normalize-space(@placeholder)=\'Username\' and normalize-space(@type)=\'text\' and normalize-space(@name)=\'username\'])[2]', 'Tab');

  await page.fill('(//input[normalize-space(@placeholder)=\'Password\' and normalize-space(@type)=\'password\' and normalize-space(@name)=\'password\'])[2]', process.env.E2E_USER_PASSWORD);

  await page.press('(//input[normalize-space(@placeholder)=\'Password\' and normalize-space(@type)=\'password\' and normalize-space(@name)=\'password\'])[2]', 'Enter');

  await page.click('input[placeholder="What have you done today?"]');
  await page.fill('input[placeholder="What have you done today?"]', 'test message');
  await page.press('input[placeholder="What have you done today?"]', 'Enter');
  await page.waitForLoadState('networkidle')

  await page.reload({waitUntil: 'networkidle'})
  await page.waitForSelector("text='test message'")

  await page.hover("//*[text()[contains(.,'test message')]]//ancestor::*[@class='doneitem']");
  await page.click("//*[text()[contains(.,'test message')]]//ancestor::*[@class='doneitem']/div[3]");

  await page.reload({waitUntil: 'networkidle'})
  const el2 = await page.$("text='test message'")
  if (el2) throw new Error('This item should have been successfully deleted.')

  await page.close();

  await context.close();
  await browser.close();
})();
