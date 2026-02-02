#!/usr/bin/env node
/**
 * Spider a Halogen SPA to discover reachable routes via navigation
 * Usage: node spider.js http://100.101.177.83
 */

import puppeteer from 'puppeteer';

const BASE_URL = process.argv[2] || 'http://100.101.177.83';
const MAX_DEPTH = 10;
const VISIT_DELAY = 300; // ms between visits to be polite
const PAGE_TIMEOUT = 30000; // 30s timeout for page loads
const RENDER_WAIT = 2000; // Wait for Halogen to render

// Track discovered routes
const discoveredRoutes = new Map(); // route -> { depth, foundFrom }
const visitedRoutes = new Set();
const routeQueue = [];

// Extract hash routes from a page
async function extractHashLinks(page) {
  return await page.evaluate(() => {
    const links = Array.from(document.querySelectorAll('a[href^="#"]'));
    return links.map(a => {
      const href = a.getAttribute('href');
      // Extract route from href="#/path" -> "/path"
      const route = href.startsWith('#/') ? href.slice(1) : href.slice(1);
      return route || '/';
    }).filter(r => r); // Remove empty
  });
}

// Normalize route for comparison
function normalizeRoute(route) {
  // Remove leading/trailing slashes, lowercase
  return route.replace(/^\/+|\/+$/g, '').toLowerCase() || 'home';
}

// Convert URL path to route name (for matching with static analysis)
function pathToRouteName(path) {
  // /howto/transitions -> HowtoTransitions
  // /tour/scrolly -> TourScrolly
  // / -> Home
  if (!path || path === '/' || path === '') return 'Home';

  const parts = path.replace(/^\/+/, '').split('/');
  return parts.map(p =>
    p.charAt(0).toUpperCase() + p.slice(1)
  ).join('');
}

async function spider() {
  console.log(`Spidering ${BASE_URL}`);
  console.log('---');

  const browser = await puppeteer.launch({
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  const page = await browser.newPage();

  // Start with homepage
  routeQueue.push({ route: '/', depth: 0, foundFrom: 'start' });
  discoveredRoutes.set('/', { depth: 0, foundFrom: 'start' });

  while (routeQueue.length > 0) {
    const { route, depth, foundFrom } = routeQueue.shift();

    if (visitedRoutes.has(route)) continue;
    if (depth > MAX_DEPTH) continue;

    visitedRoutes.add(route);

    const url = `${BASE_URL}/#${route}`;
    console.log(`[${depth}] Visiting: ${route}`);

    try {
      await page.goto(url, { waitUntil: 'networkidle0', timeout: 10000 });
      await new Promise(r => setTimeout(r, VISIT_DELAY));

      const links = await extractHashLinks(page);

      for (const link of links) {
        const normalized = '/' + normalizeRoute(link);
        if (!discoveredRoutes.has(normalized)) {
          discoveredRoutes.set(normalized, { depth: depth + 1, foundFrom: route });
          routeQueue.push({ route: normalized, depth: depth + 1, foundFrom: route });
        }
      }
    } catch (err) {
      console.log(`  Error: ${err.message}`);
    }
  }

  await browser.close();

  // Output results
  console.log('\n--- DISCOVERED ROUTES ---\n');
  console.log('route_path,route_name,depth,found_from');

  const sorted = Array.from(discoveredRoutes.entries())
    .sort((a, b) => a[1].depth - b[1].depth || a[0].localeCompare(b[0]));

  for (const [route, info] of sorted) {
    const routeName = pathToRouteName(route);
    console.log(`${route},${routeName},${info.depth},${info.foundFrom}`);
  }

  console.log(`\nTotal routes discovered: ${discoveredRoutes.size}`);

  // Write CSV file
  const csvContent = ['route_path,route_name,depth,found_from',
    ...sorted.map(([route, info]) =>
      `${route},${pathToRouteName(route)},${info.depth},${info.foundFrom}`)
  ].join('\n');

  const fs = await import('fs');
  fs.writeFileSync('discovered-routes.csv', csvContent);
  console.log('Wrote: discovered-routes.csv');
}

spider().catch(console.error);
