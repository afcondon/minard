#!/usr/bin/env node
/**
 * Compare spidered routes with static analysis results
 */

import { readFileSync } from 'fs';

// Load spidered routes
const spideredCSV = readFileSync('discovered-routes.csv', 'utf-8');
const spideredRoutes = new Set(
  spideredCSV.split('\n').slice(1).map(line => {
    const path = line.split(',')[0];
    return path;
  }).filter(Boolean)
);

// Load static route definitions
const staticCSV = readFileSync('/Users/afc/work/afc-work/PSD3-Repos/site/website/unused-routes.csv', 'utf-8');
const unusedStatic = new Set(
  staticCSV.split('\n').slice(1).map(line => line.trim()).filter(Boolean)
);

// Load all route mappings
const mappingsCSV = readFileSync('/Users/afc/work/afc-work/PSD3-Repos/site/website/route-mappings.csv', 'utf-8');
const definedRoutes = new Map();
mappingsCSV.split('\n').slice(1).forEach(line => {
  const [route, component] = line.split(',');
  if (route) definedRoutes.set(route, component);
});

// Convert route names to URL paths for comparison
function routeNameToPath(name) {
  // HowtoTransitions -> /howto/transitions
  // TourScrolly -> /tour/scrolly
  // Home -> /
  // Example -> /example (parameterized)

  if (name === 'Home') return '/';
  if (name === 'ARCHIVED') return null;

  // Split on uppercase letters
  const parts = name.replace(/([A-Z])/g, '/$1').toLowerCase().split('/').filter(Boolean);
  return '/' + parts.join('/').replace(/-/g, '-');
}

// Manual mapping for known patterns
const routeToPathMap = {
  'Home': '/',
  'GettingStarted': '/getting-started',
  'HowtoIndex': '/howto',
  'HowtoTransitions': '/howto/transitions',
  'HowtoForceGraphs': '/howto/force-graphs',
  'HowtoHierarchical': '/howto/hierarchical',
  'HowtoEvents': '/howto/events',
  'HowtoTreeAPI': '/howto/tree-api',
  'HowtoLoadingData': '/howto/loading-data',
  'HowtoAxesScales': '/howto/axes-scales',
  'HowtoTooltips': '/howto/tooltips',
  'HowtoDebugging': '/howto/debugging',
  'HowtoPerformance': '/howto/performance',
  'HowtoTreeExplorer': '/howto/tree-explorer',
  'Understanding': '/understanding',
  'UnderstandingGrammar': '/understanding/grammar',
  'UnderstandingAttributes': '/understanding/attributes',
  'UnderstandingSelections': '/understanding/selections',
  'UnderstandingTreeAPI': '/understanding/tree-api',
  'UnderstandingEmmet': '/understanding/emmet',
  'UnderstandingScenes': '/understanding/scenes',
  'Reference': '/reference',
  'ReferenceModule': '/reference/*',
  'TourIndex': '/tour',
  'TourScrolly': '/tour/scrolly',
  'TourMotionScrolly': '/tour/scrolly2-legacy',
  'TourMotionScrollyHATS': '/tour/scrolly3-hats',
  'TourFoundations': '/tour/foundations',
  'TourProfessional': '/tour/professional',
  'TourFlow': '/tour/flow',
  'TourHierarchies': '/tour/hierarchies',
  'TourMotion': '/tour/motion',
  'TourWealthHealth': '/tour/wealth-health',
  'TourInterpreters': '/tour/interpreters',
  'TourSonification': '/tour/sonification',
  'TourFPFTW': '/tour/fpftw',
  'TourGraphAlgorithms': '/tour/graph-algorithms',
  'TourLesMisGUP': '/tour/les-mis-gup',
  'TourSimpsons': '/tour/simpsons',
  'Showcase': '/showcase',
  'ShowcaseLuaEdge': '/showcase/lua-edge',
  'Simpsons': '/simpsons',
  'SimpsonsV2': '/simpsons-v2',
  'Examples': '/examples',
  'Example': '/example/*',
  'TreeAPI': '/tree-api',
  'AnimatedTreeCluster': '/animated-tree-cluster',
  'GeneralUpdatePattern': '/general-update-pattern',
  'LesMis': '/les-mis',
  'LesMisTree': '/les-mis-tree',
  'LesMisGUPTree': '/lesmis-gup-tree',
  'MermaidTreeDemo': '/mermaid-tree-demo',
  'UpdateJoinDemo': '/update-join-demo',
  'SimpleForceGraph': '/simple-force-graph',
  'SimpleChimera': '/simple-chimera',
  'SankeyDebug': '/sankey-debug',
  'ForcePlayground': '/force-playground',
  'ForcePlaygroundWASM': '/force-playground-wasm',
  'TreeBuilder': '/tree-builder',
  'TreeBuilder2': '/tree-builder-2',
  'TreeBuilder3': '/tree-builder-3',
  'SimpleTreeBuilder': '/simple-tree-builder',
  'ChartBuilder': '/chart-builder',
  'SPLOM': '/splom',
  'SPLOMHATS': '/splom-hats',
  'GUPDebug': '/gup-debug',
  'PieDonutDemo': '/pie-donut-demo',
  'VizMatrix': '/viz-matrix',
  'ForceConfigPOC': '/force-config-poc',
  'ForceConfigV2Test': '/force-config-v2-test',
  'AnimatedAttrTest': '/animated-attr-test',
  'GUPAnimatedTest': '/gup-animated-test',
  'HATSTransitionTest': '/hats-transition-test',
  'Acknowledgements': '/acknowledgements',
  'Wizard': '/wizard',
  'ModuleGraph': '/module-graph',
  'NotFound': '/not-found',
};

console.log('=== ROUTE COMPARISON ===\n');

// Find spidered routes that look like anchor links (not real routes)
const anchorLikeRoutes = [...spideredRoutes].filter(r => r.startsWith('/_'));
console.log(`Anchor-like routes (probably in-page links): ${anchorLikeRoutes.length}`);
anchorLikeRoutes.forEach(r => console.log(`  ${r}`));

// Filter out anchor routes
const realSpideredRoutes = new Set([...spideredRoutes].filter(r => !r.startsWith('/_')));
console.log(`\nReal spidered routes: ${realSpideredRoutes.size}`);

// Find defined routes and their expected paths
console.log(`\nDefined routes: ${definedRoutes.size}`);

// Build set of expected paths from defined routes
const expectedPaths = new Set();
for (const [route, component] of definedRoutes) {
  const path = routeToPathMap[route];
  if (path && !path.includes('*')) {
    expectedPaths.add(path);
  }
}

// Compare
console.log('\n--- ROUTES REACHABLE BY SPIDER BUT NOT IN STATIC MAPPING ---');
const extraSpidered = [...realSpideredRoutes].filter(r => {
  // Check if it's a parameterized route (example/*)
  if (r.startsWith('/example/')) return false;
  return !expectedPaths.has(r);
});
extraSpidered.sort().forEach(r => console.log(`  ${r}`));
console.log(`Total: ${extraSpidered.length}`);

console.log('\n--- ROUTES IN STATIC MAPPING BUT NOT SPIDERED (unreachable?) ---');
const notSpidered = [...expectedPaths].filter(p => !realSpideredRoutes.has(p));
notSpidered.sort().forEach(p => {
  // Find the route name
  const routeName = Object.entries(routeToPathMap).find(([k, v]) => v === p)?.[0] || '?';
  console.log(`  ${p} (${routeName})`);
});
console.log(`Total: ${notSpidered.length}`);

// Parameterized routes check
console.log('\n--- PARAMETERIZED ROUTES (Example/*) ---');
const exampleRoutes = [...realSpideredRoutes].filter(r => r.startsWith('/example/'));
console.log(`Found ${exampleRoutes.length} example routes via spider`);

console.log('\n--- STATIC ANALYSIS: UNUSED ROUTES ---');
console.log([...unusedStatic].join(', '));
