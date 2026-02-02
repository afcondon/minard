#!/usr/bin/env node
/**
 * Wrapper to run halogen-spider directly from output
 * (Avoids bundler issues with puppeteer's optional dependencies)
 */
import('./output/Main/index.js').then(m => m.main());
