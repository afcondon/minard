// Minimal Puppeteer FFI - just primitives, no business logic
import puppeteer from 'puppeteer';

// Launch browser with options
// Fn1 BrowserOptions (Effect (Promise BrowserHandle))
export const launchBrowserImpl = function(opts) {
  return function() {
    return puppeteer.launch({
      headless: opts.headless,
      args: opts.args
    });
  };
};

// Close browser
// Fn1 BrowserHandle (Effect (Promise Unit))
export const closeBrowserImpl = function(browser) {
  return function() {
    return browser.close();
  };
};

// Create new page
// Fn1 BrowserHandle (Effect (Promise PageHandle))
export const newPageImpl = function(browser) {
  return function() {
    return browser.newPage();
  };
};

// Close page
// Fn1 PageHandle (Effect (Promise Unit))
export const closePageImpl = function(page) {
  return function() {
    return page.close();
  };
};

// Navigate to URL
// Fn3 PageHandle String Int (Effect (Promise Unit))
export const gotoImpl = function(page, url, timeoutMs) {
  return function() {
    return page.goto(url, {
      waitUntil: 'domcontentloaded',
      timeout: timeoutMs
    }).then(() => {});  // Discard navigation response, return Unit
  };
};

// Wait for specified milliseconds
// Fn1 Int (Effect (Promise Unit))
export const waitImpl = function(ms) {
  return function() {
    return new Promise(resolve => setTimeout(resolve, ms));
  };
};

// Extract all links matching a CSS selector
// Fn2 PageHandle String (Effect (Promise (Array String)))
export const extractLinksImpl = function(page, selector) {
  return function() {
    return page.evaluate((sel) => {
      const elements = Array.from(document.querySelectorAll(sel));
      return elements.map(el => el.getAttribute('href') || '');
    }, selector);
  };
};
