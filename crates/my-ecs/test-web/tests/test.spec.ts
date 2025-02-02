import { test, expect } from '@playwright/test';

test('web test', async ({ page }) => {
  // Collects page errors.
  let errors: Array<Error> = [];
  page.on('pageerror', error => {
    errors.push(error);
  });

  // Detects errors and handles commands.
  let errMsgs: Array<string> = [];
  let expectedNumPanics = 0;
  page.on('console', msg => {
    if (msg.type() === 'error')
      errMsgs.push(msg.text());
    else if (msg.text().startsWith('playwright')) {
      const text = msg.text();
      const EXPECTED_PANICS: string = 'playwright:expectedPanics:';
      if (text.startsWith(EXPECTED_PANICS)) {
        expectedNumPanics = parseInt(text.slice(EXPECTED_PANICS.length));
      }
    } else {
      console.log(msg.text());
    }
  });

  // Prints out worker generation and destruction.
  page.on('worker', worker => {
    const now = nowString();
    const url = worker.url();
    console.log(`${now} [I] worker created: ${url}`);

    worker.on('close', worker => {
      const now = nowString();
      const url = worker.url();
      console.log(`${now} [I] worker destroyed: ${url}`);
    });
  });

  // Visits the page.
  await page.goto('./');

  // Waits for "complete" command.
  await page.waitForEvent('console', {
    predicate: msg => msg.text() === 'playwright:complete',
    timeout: 10000,
  });

  // Validates expected panic count.
  const errMsg: string = errMsgs.join('\n');
  expect(errors, errMsg).toHaveLength(expectedNumPanics);
  expect(errMsgs, errMsg).toHaveLength(expectedNumPanics);
});

// Creates a string of current time.
function nowString() {
    const now = new Date(Date.now());
    const Y = now.getFullYear().toString();
    const M = (now.getMonth() + 1).toString().padStart(2, '0');
    const D = now.getDate().toString().padStart(2, '0');
    const h = now.getHours().toString().padStart(2, '0');
    const m = now.getMinutes().toString().padStart(2, '0');
    const s = now.getSeconds().toString().padStart(2, '0');
    const ms = now.getMilliseconds().toString().padStart(3, '0');
    return `${Y}-${M}-${D} ${h}:${m}:${s}.${ms}`;
}
