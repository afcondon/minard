// ============================================================
// App — Gallery of type signature visualizations
// Uses the real PureScript CST parser (via TypeSigViz.parse)
// ============================================================

// Register init function — called by PureScript Main after parser is ready
window.__typeSigVizInit = function() {
  'use strict';

  // Guard against double-init
  if (window.__typeSigVizInitDone) return;
  window.__typeSigVizInitDone = true;

  const parse = window.TypeSigViz.parse;
  const gallery = document.getElementById('gallery');
  const nav = document.getElementById('category-nav');

  let activeCategory = null;

  function buildNav() {
    while (nav.firstChild) nav.removeChild(nav.firstChild);

    const allBtn = document.createElement('button');
    allBtn.textContent = 'All';
    allBtn.className = activeCategory === null ? 'active' : '';
    allBtn.onclick = () => { activeCategory = null; buildNav(); renderGallery(); };
    nav.appendChild(allBtn);

    const usedCategories = [...new Set(SIGNATURES.map(s => s.category))];
    for (const cat of usedCategories) {
      const meta = CATEGORIES[cat] || { label: cat, color: '#888' };
      const btn = document.createElement('button');
      btn.textContent = meta.label;
      btn.className = activeCategory === cat ? 'active' : '';
      btn.onclick = () => { activeCategory = cat; buildNav(); renderGallery(); };
      nav.appendChild(btn);
    }
  }

  function renderGallery() {
    while (gallery.firstChild) gallery.removeChild(gallery.firstChild);

    const filtered = activeCategory
      ? SIGNATURES.filter(s => s.category === activeCategory)
      : SIGNATURES;

    const grouped = new Map();
    for (const sig of filtered) {
      if (!grouped.has(sig.category)) grouped.set(sig.category, []);
      grouped.get(sig.category).push(sig);
    }

    for (const [cat, sigs] of grouped) {
      const meta = CATEGORIES[cat] || { label: cat, color: '#888' };
      const section = document.createElement('div');
      section.className = 'category-section';

      const title = document.createElement('h2');
      title.className = 'category-title';
      title.textContent = meta.label;
      title.style.color = meta.color;
      title.style.borderBottomColor = meta.color;
      section.appendChild(title);

      for (const sig of sigs) {
        section.appendChild(renderCard(sig));
      }

      gallery.appendChild(section);
    }
  }

  function renderCard(sig) {
    const card = document.createElement('div');
    card.className = 'sig-card';

    const moduleName = sig.moduleName || sig.module || '';

    // Header
    const header = document.createElement('div');
    header.className = 'sig-header';

    const name = document.createElement('span');
    name.className = 'sig-name';
    name.textContent = sig.name;
    header.appendChild(name);

    const badge = document.createElement('span');
    badge.className = 'sig-kind-badge badge-' + sig.kind;
    badge.textContent = sig.kind;
    header.appendChild(badge);

    const mod = document.createElement('span');
    mod.className = 'sig-module';
    mod.textContent = moduleName;
    header.appendChild(mod);

    card.appendChild(header);

    // ADT entries get special rendering
    if (sig.category === 'adt' && sig.constructors) {
      return renderADTCard(card, sig);
    }

    // Class definitions get special rendering
    if (sig.category === 'class_def' && sig.methods) {
      return renderClassDefCard(card, sig);
    }

    // Raw signature
    const raw = document.createElement('div');
    raw.className = 'sig-raw';
    raw.textContent = sig.name + ' :: ' + sig.sig;
    card.appendChild(raw);

    // Parse with real CST parser
    const result = parse(sig.sig);

    if (result.ok) {
      const ast = result.ast;

      // AST debug
      const details = document.createElement('details');
      details.style.marginBottom = '6px';
      const summary = document.createElement('summary');
      summary.textContent = 'AST (CST parser)';
      summary.style.fontSize = '10px';
      summary.style.color = '#bbb';
      summary.style.cursor = 'pointer';
      details.appendChild(summary);
      const pre = document.createElement('pre');
      pre.style.cssText = 'font-size:10px;color:#999;padding:8px;background:#f8f8f8;border-radius:3px;overflow:auto;max-height:300px;';
      pre.textContent = JSON.stringify(ast, null, 2);
      details.appendChild(pre);
      card.appendChild(details);

      // SVG visualization
      try {
        const vizDiv = document.createElement('div');
        vizDiv.className = 'sig-viz';
        const svg = TypeSigRenderer.renderSignature(sig.name, sig.sig, ast, {
          className: sig.className || null
        });
        vizDiv.appendChild(svg);
        card.appendChild(vizDiv);
      } catch (e) {
        const err = document.createElement('div');
        err.style.cssText = 'color:#e15759;font-size:11px;padding:8px;';
        err.textContent = 'Render error: ' + e.message;
        card.appendChild(err);
      }
    } else {
      const err = document.createElement('div');
      err.style.cssText = 'color:#e15759;font-size:11px;padding:8px;background:#fff5f5;border-radius:3px;';
      err.textContent = result.error;
      card.appendChild(err);
    }

    return card;
  }

  function renderADTCard(card, sig) {
    const raw = document.createElement('div');
    raw.className = 'sig-raw';
    const paramStr = sig.typeParams.length > 0 ? ' ' + sig.typeParams.join(' ') : '';
    const ctorStrs = sig.constructors.map(c =>
      c.name + (c.args.length > 0 ? ' ' + c.args.join(' ') : '')
    );
    raw.textContent = 'data ' + sig.name + paramStr + ' = ' + ctorStrs.join(' | ');
    card.appendChild(raw);

    try {
      const vizDiv = document.createElement('div');
      vizDiv.className = 'sig-viz';
      const svg = TypeSigRenderer.renderADT(sig.name, sig.typeParams, sig.constructors, parse);
      vizDiv.appendChild(svg);
      card.appendChild(vizDiv);
    } catch (e) {
      const err = document.createElement('div');
      err.style.cssText = 'color:#e15759;font-size:11px;padding:8px;';
      err.textContent = 'Render error: ' + e.message;
      card.appendChild(err);
    }

    return card;
  }

  function renderClassDefCard(card, sig) {
    const raw = document.createElement('div');
    raw.className = 'sig-raw';
    const superStr = sig.superclasses.length > 0
      ? sig.superclasses.join(', ') + ' <= '
      : '';
    const paramStr = sig.typeParams.length > 0 ? ' ' + sig.typeParams.join(' ') : '';
    const methodStrs = sig.methods.map(m => '  ' + m.name + ' :: ' + m.sig);
    raw.textContent = 'class ' + superStr + sig.name + paramStr + ' where\n' + methodStrs.join('\n');
    card.appendChild(raw);

    try {
      const vizDiv = document.createElement('div');
      vizDiv.className = 'sig-viz';
      const svg = TypeSigRenderer.renderClassDef(
        sig.name, sig.typeParams, sig.superclasses, sig.methods, parse
      );
      vizDiv.appendChild(svg);
      card.appendChild(vizDiv);
    } catch (e) {
      const err = document.createElement('div');
      err.style.cssText = 'color:#e15759;font-size:11px;padding:8px;';
      err.textContent = 'Render error: ' + e.message;
      card.appendChild(err);
    }

    return card;
  }

  buildNav();
  renderGallery();
  console.log('[TypeSigViz] Gallery rendered with', SIGNATURES.length, 'signatures');
};

// Fallback: if bundle.js loads before app.js or parser is already ready, trigger init
if (window.TypeSigViz && window.TypeSigViz.parse && !window.__typeSigVizInitDone) {
  window.__typeSigVizInit();
}
