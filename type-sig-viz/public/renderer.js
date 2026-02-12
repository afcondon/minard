// ============================================================
// SVG Renderer for PureScript Type Signatures
//
// Consumes AST produced by the real CST parser (via PureScript FFI).
//
// AST node shapes:
//   { tag: "typevar", name: "a" }
//   { tag: "constructor", name: "Int" }
//   { tag: "applied", constructor: <node>, args: [<node>...] }
//   { tag: "function", params: [<node>...], returnType: <node> }
//   { tag: "forall", vars: ["a","b"], body: <node> }
//   { tag: "constrained", constraints: [{tag:"constraint", name:"Ord", args:[...]}], body: <node> }
//   { tag: "record", fields: [{tag:"field", name:"x", type:<node>}], rowVar: "r"|null }
//   { tag: "row",    fields: [{tag:"field", name:"x", type:<node>}], rowVar: "r"|null }
//   { tag: "parens", inner: <node> }
//   { tag: "operator", left: <node>, op: "+", right: <node> }
//   { tag: "kinded", type: <node>, kind: <node> }
// ============================================================

const TypeSigRenderer = (() => {

  const FONT_SIZE = 13;
  const FONT_FAMILY = "'Fira Code', 'SF Mono', 'Consolas', monospace";
  const CHAR_WIDTH = 7.8;
  const LINE_HEIGHT = 20;
  const PADDING = { x: 8, y: 6 };
  const ARROW = '\u2192';       // →
  const FORALL = '\u2200';      // ∀
  const ARROW_W = CHAR_WIDTH * 3.5; // wider: space + space + → + space

  const COLORS = {
    keyword:     '#7b61ff',
    arrow:       '#71717a',
    constraint:  '#92400e',
    constraintBg:'#fef3c7',
    constraintBd:'#d97706',
    typevar:     '#0369a1',
    constructor: '#16653e',
    effect:      '#9333ea',
    paren:       '#aaa',
    separator:   '#aaa',
    name:        '#222',
    fieldName:   '#333',
    fieldType:   '#555',
    recordBorder:'#ccc',
    rowVar:      '#0369a1',
    hktBg:       '#eff6ff',
    hktBd:       '#3b82f6',
    hktText:     '#1d4ed8',
    classBg:     '#f0f4ff',
    classBd:     '#6366f1',
    classText:   '#4338ca',
  };

  // Type variable background colors (white text on colored bg)
  // Tailwind 600 level — good contrast with white
  const TYPE_VAR_PALETTE = [
    '#0284c7', // sky
    '#d97706', // amber
    '#059669', // emerald
    '#7c3aed', // violet
    '#dc2626', // red
    '#0d9488', // teal
    '#c026d3', // fuchsia
    '#4f46e5', // indigo
  ];

  const EFFECTS = new Set(['Effect', 'Aff', 'MonadAff', 'MonadEffect']);

  // ── Per-render state ──────────────────────────────────────

  let _varColors = {};   // var name → color, set per signature

  function assignVarColors(vars) {
    const colors = {};
    const unique = [...new Set(vars)];
    for (let i = 0; i < unique.length; i++) {
      colors[unique[i]] = TYPE_VAR_PALETTE[i % TYPE_VAR_PALETTE.length];
    }
    return colors;
  }

  // Collect ALL forall vars from an AST (including nested foralls for rank-N)
  function collectAllVars(ast) {
    const vars = [];
    function walk(node) {
      if (!node) return;
      switch (node.tag) {
        case 'forall':
          vars.push(...node.vars);
          walk(node.body);
          break;
        case 'constrained':
          walk(node.body);
          break;
        case 'function':
          for (const p of node.params) walk(p);
          walk(node.returnType);
          break;
        case 'applied':
          walk(node.constructor);
          for (const a of node.args) walk(a);
          break;
        case 'parens':
          walk(node.inner);
          break;
        case 'record':
        case 'row':
          for (const f of (node.fields || [])) walk(f.type);
          break;
      }
    }
    walk(ast);
    return vars;
  }

  // Parse className string like "Foldable f" → { name: "Foldable", params: ["f"] }
  function parseClassName(className) {
    if (!className) return null;
    const parts = className.trim().split(/\s+/);
    return { name: parts[0], params: parts.slice(1) };
  }

  // ── Helpers ─────────────────────────────────────────────────

  function textWidth(str) { return str.length * CHAR_WIDTH; }

  function svgEl(tag, attrs, children) {
    const el = document.createElementNS('http://www.w3.org/2000/svg', tag);
    for (const [k, v] of Object.entries(attrs || {})) el.setAttribute(k, v);
    for (const child of (children || [])) {
      if (typeof child === 'string') el.textContent = child;
      else if (child) el.appendChild(child);
    }
    return el;
  }

  function textEl(x, y, text, style) {
    return svgEl('text', {
      x, y,
      'font-family': FONT_FAMILY,
      'font-size': FONT_SIZE,
      'dominant-baseline': 'middle',
      style: style || ''
    }, [text]);
  }

  function getNodeName(node) {
    if (!node) return '?';
    if (node.tag === 'typevar') return node.name;
    if (node.tag === 'constructor') return node.name;
    return '?';
  }

  function isEffectName(name) { return EFFECTS.has(name); }

  function isTypeVarNode(node) {
    return node && node.tag === 'typevar';
  }

  // Type variable pill dimensions
  function varPillWidth(name) {
    return textWidth(name) + 10;  // 5px padding each side
  }

  function renderVarPill(g, x, y, name, fontSize) {
    const color = _varColors[name] || COLORS.typevar;
    const fs = fontSize || FONT_SIZE;
    const pw = textWidth(name) + 10;
    const ph = fs + 5;
    const py = y + LINE_HEIGHT / 2 - ph / 2;

    g.appendChild(svgEl('rect', {
      x, y: py, width: pw, height: ph,
      rx: 3, ry: 3,
      style: `fill:${color};`
    }));
    g.appendChild(textEl(x + 5, y + LINE_HEIGHT / 2, name,
      `fill:white;font-weight:600;font-style:italic;font-size:${fs}px;`));
    return pw;
  }

  // Small var pill for annotations (forall row, class row)
  function renderSmallPill(g, x, y, name, centerY) {
    const color = _varColors[name] || COLORS.typevar;
    const pw = textWidth(name) + 8;
    const ph = 16;
    const py = centerY - ph / 2;
    g.appendChild(svgEl('rect', {
      x, y: py, width: pw, height: ph,
      rx: 3, ry: 3,
      style: `fill:${color};`
    }));
    g.appendChild(textEl(x + 4, centerY, name,
      `fill:white;font-weight:600;font-style:italic;font-size:11px;`));
    return pw;
  }

  // ── Measure ─────────────────────────────────────────────────

  function measure(node) {
    if (!node) return { width: 0, height: 0 };

    switch (node.tag) {
      case 'typevar':
        return { width: varPillWidth(node.name), height: LINE_HEIGHT };

      case 'constructor':
        return { width: textWidth(node.name), height: LINE_HEIGHT };

      case 'applied': {
        const headW = measure(node.constructor).width;
        let argsW = 0;
        let maxH = LINE_HEIGHT;
        for (const arg of node.args) {
          const am = measure(arg);
          argsW += CHAR_WIDTH + am.width;
          maxH = Math.max(maxH, am.height);
        }
        return { width: headW + argsW, height: maxH };
      }

      case 'function': {
        let w = 0;
        for (const p of node.params) {
          const pm = measure(p);
          w += pm.width + ARROW_W;
        }
        w += measure(node.returnType).width;
        return { width: w, height: Math.max(LINE_HEIGHT, ...node.params.map(p => measure(p).height), measure(node.returnType).height) };
      }

      case 'record':
      case 'row':
        return measureTable(node.fields, node.rowVar);

      case 'forall': {
        // Inline forall for rank-N: ∀ vars . body (with dotted outline padding)
        let w = 10; // 5px padding each side
        w += CHAR_WIDTH + 2; // ∀ symbol
        for (const v of node.vars) w += varPillWidth(v) + 3;
        w += CHAR_WIDTH; // dot separator
        const bodyM = measure(node.body);
        w += bodyM.width;
        return { width: w, height: bodyM.height };
      }

      case 'constrained': {
        const bodyM = measure(node.body);
        const cH = node.constraints.length * 24 + 10;
        return { width: bodyM.width, height: bodyM.height + cH };
      }

      case 'parens': {
        const m = measure(node.inner);
        return { width: m.width + 2 * CHAR_WIDTH, height: m.height };
      }

      case 'operator': {
        const lW = measure(node.left).width;
        const rW = measure(node.right).width;
        return { width: lW + textWidth(' ' + node.op + ' ') + rW, height: LINE_HEIGHT };
      }

      case 'kinded': {
        const tW = measure(node.type).width;
        const kW = measure(node.kind).width;
        return { width: tW + textWidth(' :: ') + kW, height: LINE_HEIGHT };
      }

      case 'field':
        return measure(node.type);

      case 'constraint':
        return { width: textWidth(constraintText(node)), height: LINE_HEIGHT };

      default:
        return { width: 40, height: LINE_HEIGHT };
    }
  }

  function measureTable(fields, rowVar) {
    if (!fields || fields.length === 0) {
      return rowVar ? { width: 80, height: 50 } : { width: 40, height: 30 };
    }
    let maxNameW = 0, maxTypeW = 0;
    let totalRowH = 0;
    for (const f of fields) {
      maxNameW = Math.max(maxNameW, textWidth(f.name));
      const ftm = measureFieldFull(f.type);
      maxTypeW = Math.max(maxTypeW, ftm.width);
      totalRowH += Math.max(LINE_HEIGHT + 4, ftm.height + 6);
    }
    const colSepW = textWidth(' :: ');
    const tableW = PADDING.x * 2 + maxNameW + colSepW + maxTypeW + PADDING.x;
    const rowVarH = rowVar ? 28 : 0;
    return { width: Math.max(tableW, 80), height: totalRowH + rowVarH + PADDING.y * 2 };
  }

  function measureFieldType(node) {
    return measureFieldFull(node).width;
  }

  function measureFieldFull(node) {
    if (!node) return { width: 40, height: LINE_HEIGHT };
    if (node.tag === 'typevar') return { width: varPillWidth(node.name), height: LINE_HEIGHT };
    if (node.tag === 'constructor') return { width: textWidth(node.name), height: LINE_HEIGHT };
    if (node.tag === 'record' || node.tag === 'row') return measureTable(node.fields, node.rowVar);
    if (node.tag === 'applied') {
      let w = measure(node.constructor).width;
      let maxH = LINE_HEIGHT;
      for (const a of node.args) {
        const am = measureFieldFull(a);
        w += CHAR_WIDTH + am.width;
        maxH = Math.max(maxH, am.height);
      }
      return { width: w, height: maxH };
    }
    if (node.tag === 'parens') {
      const m = measureFieldFull(node.inner);
      return { width: m.width + 2 * CHAR_WIDTH, height: m.height };
    }
    if (node.tag === 'function') {
      let w = 0;
      for (const p of node.params) w += measureFieldFull(p).width + ARROW_W;
      w += measureFieldFull(node.returnType).width;
      return { width: w, height: LINE_HEIGHT };
    }
    return { width: typeToText(node).length * CHAR_WIDTH, height: LINE_HEIGHT };
  }

  function constraintText(c) {
    let text = c.name;
    if (c.args && c.args.length > 0) {
      text += ' ' + c.args.map(typeToText).join(' ');
    }
    return text;
  }

  // ── Type to plain text ──────────────────────────────────────

  function typeToText(node) {
    if (!node) return '?';
    switch (node.tag) {
      case 'typevar': return node.name;
      case 'constructor': return node.name;
      case 'applied': return typeToText(node.constructor) + ' ' + node.args.map(typeToText).join(' ');
      case 'function': return node.params.map(typeToText).join(' -> ') + ' -> ' + typeToText(node.returnType);
      case 'parens': return '(' + typeToText(node.inner) + ')';
      case 'record': return '{ ' + fieldsToText(node.fields, node.rowVar) + ' }';
      case 'row': return '( ' + fieldsToText(node.fields, node.rowVar) + ' )';
      case 'forall': return 'forall ' + node.vars.join(' ') + '. ' + typeToText(node.body);
      case 'constrained': return node.constraints.map(constraintText).join(' => ') + ' => ' + typeToText(node.body);
      case 'operator': return typeToText(node.left) + ' ' + node.op + ' ' + typeToText(node.right);
      case 'kinded': return typeToText(node.type) + ' :: ' + typeToText(node.kind);
      default: return '?';
    }
  }

  function fieldsToText(fields, rowVar) {
    const fs = (fields || []).map(f => f.name + ' :: ' + typeToText(f.type)).join(', ');
    return rowVar ? fs + ' | ' + rowVar : fs;
  }

  // ── Render: main entry ──────────────────────────────────────

  function renderSignature(name, sig, ast, options) {
    const className = options && options.className;
    const classInfo = parseClassName(className);

    const { forallVars, constraints, body } = unwrapType(ast);

    // Collect all vars including nested foralls and class params
    const classParams = classInfo ? classInfo.params : [];
    const allVars = [...classParams, ...collectAllVars(ast)];
    // Top-level forall vars go after class params for consistent ordering
    _varColors = assignVarColors([...classParams, ...forallVars, ...allVars]);

    const nameW = textWidth(name + ' :: ');
    const bodyM = measure(body);

    // Vertical layout: class row → constraint pile → name+body → forall (under name)
    const classH = classInfo ? 20 : 0;
    const forallH = forallVars.length > 0 ? 22 : 0;
    const constraintH = constraints.length > 0 ? constraints.length * 24 + 10 : 0;

    const bodyH = Math.max(bodyM.height, LINE_HEIGHT);
    const totalW = nameW + bodyM.width + 40;
    const totalH = classH + constraintH + bodyH + forallH + 16;

    const svg = svgEl('svg', {
      width: Math.max(totalW, 100),
      height: Math.max(totalH, 36),
      viewBox: `0 0 ${Math.max(totalW, 100)} ${Math.max(totalH, 36)}`,
      style: 'overflow: visible;'
    });

    const g = svgEl('g', { transform: 'translate(10, 8)' });
    svg.appendChild(g);

    let curY = 0;

    // Class membership annotation
    if (classInfo) {
      let cx = 0;
      // "class" keyword
      g.appendChild(textEl(cx, curY + 10, 'class',
        `fill:${COLORS.classBd};font-weight:600;font-size:10px;letter-spacing:0.5px;`));
      cx += textWidth('class') + 6;
      // Class name
      g.appendChild(textEl(cx, curY + 10, classInfo.name,
        `fill:${COLORS.classText};font-weight:700;font-size:11px;`));
      cx += textWidth(classInfo.name) + 4;
      // Class params as pills
      for (const p of classInfo.params) {
        cx += renderSmallPill(g, cx, curY, p, curY + 10);
        cx += 3;
      }
      curY += classH;
    }

    // Constraint pile (above the body, aligned with body start)
    if (constraints.length > 0) {
      renderConstraintPile(g, nameW, curY, constraints);
      curY += constraintH;
    }

    // Declaration name
    g.appendChild(textEl(0, curY + LINE_HEIGHT / 2, name,
      `fill:${COLORS.name};font-weight:700;`));

    // ::
    g.appendChild(textEl(textWidth(name), curY + LINE_HEIGHT / 2, ' :: ',
      `fill:${COLORS.separator};`));

    // Body
    renderNode(g, nameW, curY, body);

    curY += bodyH;

    // ∀ annotation with colored variable pills (underneath the identifier)
    if (forallVars.length > 0) {
      curY += 4; // breathing room between body and forall
      let fx = 0;
      g.appendChild(textEl(fx, curY + 9, FORALL,
        `fill:${COLORS.keyword};font-weight:700;font-size:14px;`));
      fx += CHAR_WIDTH * 1.4 + 3;

      for (let i = 0; i < forallVars.length; i++) {
        if (i > 0) fx += 3;
        fx += renderSmallPill(g, fx, curY + 1, forallVars[i], curY + 9);
      }
    }

    _varColors = {};
    return svg;
  }

  function unwrapType(ast) {
    let forallVars = [];
    let constraints = [];
    let body = ast;

    while (body) {
      if (body.tag === 'forall') {
        forallVars = forallVars.concat(body.vars);
        body = body.body;
      } else if (body.tag === 'constrained') {
        constraints = constraints.concat(body.constraints);
        body = body.body;
      } else {
        break;
      }
    }

    return { forallVars, constraints, body };
  }

  // ── Render: constraint pile ─────────────────────────────────

  function renderConstraintPile(g, x, y, constraints) {
    for (let i = 0; i < constraints.length; i++) {
      const c = constraints[i];
      const text = constraintText(c);
      const w = textWidth(text) + 16;
      const pillY = y + i * 24;

      g.appendChild(svgEl('rect', {
        x, y: pillY, width: w, height: 20,
        rx: 3, ry: 3,
        style: `fill:${COLORS.constraintBg};stroke:${COLORS.constraintBd};stroke-width:1;`
      }));
      g.appendChild(textEl(x + 8, pillY + 10, text,
        `fill:${COLORS.constraint};font-weight:600;font-size:11px;`));
    }

    // Connecting dashed line
    const pileBottom = y + constraints.length * 24;
    g.appendChild(svgEl('line', {
      x1: x + 4, y1: pileBottom, x2: x + 4, y2: pileBottom + 8,
      stroke: COLORS.constraintBd, 'stroke-width': 1, 'stroke-dasharray': '2 2'
    }));
  }

  // ── Render: arrow glyph ───────────────────────────────────

  function renderArrow(g, x, y) {
    g.appendChild(textEl(x + CHAR_WIDTH * 1.0, y + LINE_HEIGHT / 2, ARROW,
      `fill:${COLORS.arrow};font-weight:400;font-size:16px;`));
    return ARROW_W;
  }

  // ── Render: AST nodes ───────────────────────────────────────

  function renderNode(g, x, y, node) {
    if (!node) return 0;

    switch (node.tag) {
      case 'typevar':
        return renderVarPill(g, x, y, node.name);

      case 'constructor': {
        const col = isEffectName(node.name) ? COLORS.effect : COLORS.constructor;
        g.appendChild(textEl(x, y + LINE_HEIGHT / 2, node.name,
          `fill:${col};font-weight:600;`));
        return textWidth(node.name);
      }

      case 'applied':
        return renderApplied(g, x, y, node);

      case 'function':
        return renderFunction(g, x, y, node);

      case 'record':
        return renderRecord(g, x, y, node, false);

      case 'row':
        return renderRecord(g, x, y, node, true);

      case 'forall':
        // Nested forall (rank-N) — render inline
        return renderInlineForall(g, x, y, node);

      case 'constrained': {
        const cH = node.constraints.length * 24 + 10;
        renderConstraintPile(g, x, y, node.constraints);
        return renderNode(g, x, y + cH, node.body);
      }

      case 'parens':
        return renderParens(g, x, y, node);

      case 'operator':
        return renderOperator(g, x, y, node);

      case 'kinded':
        return renderKinded(g, x, y, node);

      default:
        g.appendChild(textEl(x, y + LINE_HEIGHT / 2, '?', `fill:#999;`));
        return CHAR_WIDTH;
    }
  }

  // ── Render: inline forall (for rank-N types) ──────────────

  function renderInlineForall(g, x, y, node) {
    const pad = 5;
    let cx = x + pad;

    // ∀ symbol
    g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, FORALL,
      `fill:${COLORS.keyword};font-weight:700;font-size:12px;`));
    cx += CHAR_WIDTH + 2;

    // Variable pills
    for (let i = 0; i < node.vars.length; i++) {
      if (i > 0) cx += 2;
      cx += renderVarPill(g, cx, y, node.vars[i], 11);
      cx += 1;
    }

    // Dot separator
    g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, '.',
      `fill:${COLORS.keyword};font-size:12px;font-weight:600;`));
    cx += CHAR_WIDTH;

    // Body
    cx += renderNode(g, cx, y, node.body);

    cx += pad;
    const totalW = cx - x;

    // Dotted outline around entire rank-N expression
    g.appendChild(svgEl('rect', {
      x, y: y - 2, width: totalW, height: LINE_HEIGHT + 4,
      rx: 4, ry: 4,
      style: 'fill:rgba(123,97,255,0.04);stroke:#7b61ff;stroke-width:1;stroke-dasharray:3 2;'
    }));

    return totalW;
  }

  function renderApplied(g, x, y, node) {
    let cx = x;
    const headName = getNodeName(node.constructor);
    const isHKT = isTypeVarNode(node.constructor);
    const totalW = measure(node).width;

    // HKT highlight box
    if (isHKT) {
      g.appendChild(svgEl('rect', {
        x: cx - 2, y: y - 1, width: totalW + 4, height: LINE_HEIGHT + 2,
        rx: 3, ry: 3,
        style: `fill:${COLORS.hktBg};stroke:${COLORS.hktBd};stroke-width:1;`
      }));
    }

    // Head
    if (isHKT) {
      cx += renderVarPill(g, cx, y, headName);
    } else {
      const headColor = isEffectName(headName) ? COLORS.effect : COLORS.constructor;
      g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, headName,
        `fill:${headColor};font-weight:600;`));
      cx += textWidth(headName);
    }

    // Args
    for (const arg of node.args) {
      cx += CHAR_WIDTH;
      cx += renderNode(g, cx, y, arg);
    }

    return cx - x;
  }

  function renderFunction(g, x, y, node) {
    let cx = x;

    for (let i = 0; i < node.params.length; i++) {
      const param = node.params[i];

      if (param.tag === 'record' || param.tag === 'row') {
        cx += renderRecord(g, cx, y, param, param.tag === 'row');
      } else {
        cx += renderNode(g, cx, y, param);
      }

      cx += renderArrow(g, cx, y);
    }

    const ret = node.returnType;
    if (ret.tag === 'record' || ret.tag === 'row') {
      cx += renderRecord(g, cx, y, ret, ret.tag === 'row');
    } else {
      cx += renderNode(g, cx, y, ret);
    }

    return cx - x;
  }

  function renderRecord(g, x, y, node, isRowType) {
    const fields = node.fields || [];
    const isOpen = !!node.rowVar;

    if (fields.length === 0 && !isOpen) {
      const label = isRowType ? '()' : '{}';
      g.appendChild(textEl(x, y + LINE_HEIGHT / 2, label, `fill:${COLORS.paren};`));
      return textWidth(label);
    }

    let maxNameW = 0, maxTypeW = 0;
    const rowHeights = [];
    for (const f of fields) {
      maxNameW = Math.max(maxNameW, textWidth(f.name));
      const ftm = measureFieldFull(f.type);
      maxTypeW = Math.max(maxTypeW, ftm.width);
      rowHeights.push(Math.max(LINE_HEIGHT + 4, ftm.height + 6));
    }

    const colSepW = textWidth(' :: ');
    const tableW = Math.max(PADDING.x * 2 + maxNameW + colSepW + maxTypeW + PADDING.x, 80);
    const totalRowH = rowHeights.reduce((a, b) => a + b, 0);
    const rowVarH = isOpen ? 28 : 0;
    const tableH = totalRowH + rowVarH + PADDING.y * 2;

    const dashStyle = isOpen ? 'stroke-dasharray:5 3;' : '';
    g.appendChild(svgEl('rect', {
      x, y, width: tableW, height: tableH,
      rx: 4, ry: 4,
      style: `fill:white;stroke:${COLORS.recordBorder};stroke-width:${isOpen ? 1.5 : 1};${dashStyle}`
    }));

    let currentY = y + PADDING.y;
    for (let i = 0; i < fields.length; i++) {
      const f = fields[i];
      const rh = rowHeights[i];

      if (i > 0) {
        g.appendChild(svgEl('line', {
          x1: x + 4, y1: currentY - 2,
          x2: x + tableW - 4, y2: currentY - 2,
          stroke: '#f0f0f0', 'stroke-width': 1
        }));
      }

      g.appendChild(textEl(x + PADDING.x, currentY + LINE_HEIGHT / 2, f.name,
        `fill:${COLORS.fieldName};font-weight:600;font-size:12px;`));

      g.appendChild(textEl(x + PADDING.x + maxNameW + 4, currentY + LINE_HEIGHT / 2, '::',
        `fill:${COLORS.separator};font-size:12px;`));

      renderFieldTypeNode(g, x + PADDING.x + maxNameW + colSepW, currentY, f.type);

      currentY += rh;
    }

    if (isOpen && node.rowVar) {
      const ellY = currentY + 4;
      const cx = x + tableW / 2;
      for (let d = 0; d < 3; d++) {
        g.appendChild(svgEl('circle', {
          cx, cy: ellY + d * 6 + 3, r: 1.5,
          fill: _varColors[node.rowVar] || COLORS.rowVar
        }));
      }
      const rvColor = _varColors[node.rowVar] || COLORS.rowVar;
      const rvW = textWidth(node.rowVar) + 8;
      const rvX = cx + 10;
      g.appendChild(svgEl('rect', {
        x: rvX, y: ellY + 3, width: rvW, height: 14,
        rx: 3, ry: 3,
        style: `fill:${rvColor};`
      }));
      g.appendChild(textEl(rvX + 4, ellY + 10, node.rowVar,
        `fill:white;font-style:italic;font-size:10px;font-weight:600;`));
    }

    return tableW;
  }

  function renderFieldTypeNode(g, x, y, node) {
    if (!node) return;

    if (node.tag === 'record' || node.tag === 'row') {
      renderRecord(g, x, y, node, node.tag === 'row');
      return;
    }

    if (node.tag === 'typevar') {
      const color = _varColors[node.name] || COLORS.typevar;
      const pw = textWidth(node.name) + 8;
      const ph = 15;
      const py = y + LINE_HEIGHT / 2 - ph / 2;
      g.appendChild(svgEl('rect', {
        x, y: py, width: pw, height: ph,
        rx: 3, ry: 3,
        style: `fill:${color};`
      }));
      g.appendChild(textEl(x + 4, y + LINE_HEIGHT / 2, node.name,
        `fill:white;font-weight:600;font-style:italic;font-size:11px;`));
    } else if (node.tag === 'constructor') {
      const col = isEffectName(node.name) ? COLORS.effect : COLORS.constructor;
      g.appendChild(textEl(x, y + LINE_HEIGHT / 2, node.name,
        `fill:${col};font-weight:600;font-size:12px;`));
    } else if (node.tag === 'applied') {
      let cx = x;
      const headName = getNodeName(node.constructor);
      const isHKT = isTypeVarNode(node.constructor);
      if (isHKT) {
        const color = _varColors[headName] || COLORS.typevar;
        const pw = textWidth(headName) + 8;
        const ph = 15;
        g.appendChild(svgEl('rect', {
          x: cx, y: y + LINE_HEIGHT / 2 - ph / 2, width: pw, height: ph,
          rx: 3, ry: 3,
          style: `fill:${color};`
        }));
        g.appendChild(textEl(cx + 4, y + LINE_HEIGHT / 2, headName,
          `fill:white;font-weight:600;font-style:italic;font-size:11px;`));
        cx += pw;
      } else {
        const col = isEffectName(headName) ? COLORS.effect : COLORS.constructor;
        g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, headName,
          `fill:${col};font-weight:600;font-size:12px;`));
        cx += textWidth(headName);
      }
      for (const arg of node.args) {
        cx += CHAR_WIDTH * 0.8;
        renderFieldTypeNode(g, cx, y, arg);
        cx += measureFieldType(arg);
      }
    } else if (node.tag === 'parens') {
      g.appendChild(textEl(x, y + LINE_HEIGHT / 2, '(',
        `fill:${COLORS.paren};font-size:12px;`));
      renderFieldTypeNode(g, x + CHAR_WIDTH, y, node.inner);
      const innerW = measureFieldType(node.inner);
      g.appendChild(textEl(x + CHAR_WIDTH + innerW, y + LINE_HEIGHT / 2, ')',
        `fill:${COLORS.paren};font-size:12px;`));
    } else if (node.tag === 'function') {
      let cx = x;
      for (const p of node.params) {
        renderFieldTypeNode(g, cx, y, p);
        cx += measureFieldType(p);
        g.appendChild(textEl(cx + CHAR_WIDTH * 0.5, y + LINE_HEIGHT / 2, ARROW,
          `fill:${COLORS.arrow};font-size:12px;`));
        cx += ARROW_W;
      }
      renderFieldTypeNode(g, cx, y, node.returnType);
    } else {
      const text = typeToText(node);
      g.appendChild(textEl(x, y + LINE_HEIGHT / 2, text,
        `fill:${COLORS.fieldType};font-size:12px;`));
    }
  }

  function renderParens(g, x, y, node) {
    let cx = x;
    g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, '(', `fill:${COLORS.paren};`));
    cx += CHAR_WIDTH;
    cx += renderNode(g, cx, y, node.inner);
    g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, ')', `fill:${COLORS.paren};`));
    cx += CHAR_WIDTH;
    return cx - x;
  }

  function renderOperator(g, x, y, node) {
    let cx = x;
    cx += renderNode(g, cx, y, node.left);
    const opText = ' ' + node.op + ' ';
    g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, opText,
      `fill:${COLORS.arrow};font-weight:600;`));
    cx += textWidth(opText);
    cx += renderNode(g, cx, y, node.right);
    return cx - x;
  }

  function renderKinded(g, x, y, node) {
    let cx = x;
    cx += renderNode(g, cx, y, node.type);
    g.appendChild(textEl(cx, y + LINE_HEIGHT / 2, ' :: ',
      `fill:${COLORS.separator};`));
    cx += textWidth(' :: ');
    cx += renderNode(g, cx, y, node.kind);
    return cx - x;
  }

  // ── Render: ADT (data type with constructors) ──────────────

  const ADT_COLORS = {
    headerBg:      '#fef3c7',
    headerBd:      '#d97706',
    headerText:    '#92400e',
    rail:          '#d97706',
    ctorName:      '#b45309',
    ctorBranch:    '#d97706',
  };

  const ADT_ROW_HEIGHT = 28;
  const ADT_RAIL_X = 16;
  const ADT_CTOR_X = 40;

  function renderADT(name, typeParams, constructors, parseFn) {
    _varColors = assignVarColors(typeParams);

    const parsedCtors = constructors.map(ctor => ({
      name: ctor.name,
      parsedArgs: ctor.args.map(argStr => {
        const result = parseFn(argStr);
        return result.ok ? result.ast : null;
      })
    }));

    const headerText = 'data ' + name + (typeParams.length > 0 ? ' ' + typeParams.join(' ') : '');
    const headerW = textWidth(headerText) + 24;

    let maxCtorNameW = 0;
    for (const ctor of parsedCtors) {
      maxCtorNameW = Math.max(maxCtorNameW, textWidth(ctor.name));
    }

    let maxArgsW = 0;
    for (const ctor of parsedCtors) {
      let argsW = 0;
      for (let i = 0; i < ctor.parsedArgs.length; i++) {
        if (i > 0) argsW += CHAR_WIDTH;
        argsW += ctor.parsedArgs[i] ? measure(ctor.parsedArgs[i]).width : 30;
      }
      maxArgsW = Math.max(maxArgsW, argsW);
    }

    const ctorX = ADT_CTOR_X;
    const argX = ctorX + maxCtorNameW + CHAR_WIDTH * 2;
    const totalW = Math.max(headerW, argX + maxArgsW + 20, 180);
    const headerH = 28;
    const ctorAreaTop = headerH + 10;
    const totalH = ctorAreaTop + parsedCtors.length * ADT_ROW_HEIGHT + 8;

    const svg = svgEl('svg', {
      width: totalW,
      height: totalH,
      viewBox: `0 0 ${totalW} ${totalH}`,
      style: 'overflow: visible;'
    });

    const g = svgEl('g', {});
    svg.appendChild(g);

    g.appendChild(svgEl('rect', {
      x: 0, y: 0, width: totalW, height: headerH,
      rx: 4, ry: 4,
      style: `fill:${ADT_COLORS.headerBg};stroke:${ADT_COLORS.headerBd};stroke-width:1;`
    }));

    g.appendChild(textEl(8, headerH / 2, 'data ',
      `fill:${COLORS.keyword};font-weight:600;font-size:12px;`));

    const dataKwW = textWidth('data ');
    g.appendChild(textEl(8 + dataKwW, headerH / 2, name,
      `fill:${ADT_COLORS.headerText};font-weight:700;font-size:${FONT_SIZE}px;`));

    let paramX = 8 + dataKwW + textWidth(name) + 6;
    for (const p of typeParams) {
      paramX += renderSmallPill(g, paramX, 6, p, headerH / 2);
      paramX += 3;
    }

    const railTop = ctorAreaTop;
    const railBottom = ctorAreaTop + (parsedCtors.length - 1) * ADT_ROW_HEIGHT;
    if (parsedCtors.length > 1) {
      g.appendChild(svgEl('line', {
        x1: ADT_RAIL_X, y1: railTop + ADT_ROW_HEIGHT / 2,
        x2: ADT_RAIL_X, y2: railBottom + ADT_ROW_HEIGHT / 2,
        stroke: ADT_COLORS.rail, 'stroke-width': 2,
        'stroke-linecap': 'round'
      }));
    }

    for (let i = 0; i < parsedCtors.length; i++) {
      const ctor = parsedCtors[i];
      const cy = ctorAreaTop + i * ADT_ROW_HEIGHT + ADT_ROW_HEIGHT / 2;

      g.appendChild(svgEl('line', {
        x1: ADT_RAIL_X, y1: cy,
        x2: ctorX - 4, y2: cy,
        stroke: ADT_COLORS.ctorBranch, 'stroke-width': 1.5,
        'stroke-linecap': 'round'
      }));

      g.appendChild(svgEl('circle', {
        cx: ADT_RAIL_X, cy,
        r: 3,
        fill: 'white', stroke: ADT_COLORS.rail, 'stroke-width': 1.5
      }));

      g.appendChild(textEl(ctorX, cy, ctor.name,
        `fill:${ADT_COLORS.ctorName};font-weight:700;`));

      let ax = argX;
      for (let j = 0; j < ctor.parsedArgs.length; j++) {
        const ast = ctor.parsedArgs[j];
        if (j > 0) ax += CHAR_WIDTH;
        if (ast) {
          ax += renderNode(g, ax, cy - LINE_HEIGHT / 2, ast);
        }
      }

      if (ctor.parsedArgs.length === 0) {
        g.appendChild(svgEl('line', {
          x1: argX, y1: cy,
          x2: argX + 12, y2: cy,
          stroke: '#ddd', 'stroke-width': 1, 'stroke-dasharray': '2 2'
        }));
      }
    }

    _varColors = {};
    return svg;
  }

  // ── Render: Class definition ──────────────────────────────

  function renderClassDef(name, typeParams, superclasses, methods, parseFn) {
    _varColors = assignVarColors(typeParams);

    // Parse method signatures
    const parsedMethods = methods.map(m => {
      const result = parseFn(m.sig);
      return { name: m.name, sig: m.sig, ast: result.ok ? result.ast : null, error: result.ok ? null : result.error };
    });

    // Measure
    const headerH = 28;
    const superH = superclasses.length > 0 ? 22 : 0;

    // Calculate actual height per method
    let totalMethodH = 0;
    let maxMethodW = textWidth('class ' + name) + 80;
    for (const m of parsedMethods) {
      let mh = 8; // top padding
      if (m.ast) {
        const { forallVars, constraints, body } = unwrapType(m.ast);
        const bodyM = measure(body);
        const bodyH = Math.max(LINE_HEIGHT, bodyM.height);
        const mNameW = textWidth(m.name + ' :: ');
        maxMethodW = Math.max(maxMethodW, mNameW + bodyM.width + 40);
        if (constraints.length > 0) mh += 20;  // constraint row
        mh += bodyH;                            // name + body
        if (forallVars.length > 0) mh += 21;   // forall underneath + breathing room
        mh += 8; // bottom padding
      } else {
        mh += 24;
      }
      totalMethodH += mh;
    }
    if (parsedMethods.length === 0) totalMethodH = 30;

    const totalH = headerH + superH + totalMethodH + 16;
    const totalW = Math.max(maxMethodW, 200);

    const svg = svgEl('svg', {
      width: totalW,
      height: totalH,
      viewBox: `0 0 ${totalW} ${totalH}`,
      style: 'overflow: visible;'
    });

    const g = svgEl('g', {});
    svg.appendChild(g);

    // Header bar
    g.appendChild(svgEl('rect', {
      x: 0, y: 0, width: totalW, height: headerH,
      rx: 4, ry: 4,
      style: `fill:${COLORS.classBg};stroke:${COLORS.classBd};stroke-width:1;`
    }));

    let hx = 8;
    g.appendChild(textEl(hx, headerH / 2, 'class ',
      `fill:${COLORS.keyword};font-weight:600;font-size:12px;`));
    hx += textWidth('class ');

    g.appendChild(textEl(hx, headerH / 2, name,
      `fill:${COLORS.classText};font-weight:700;font-size:${FONT_SIZE}px;`));
    hx += textWidth(name) + 6;

    for (const p of typeParams) {
      hx += renderSmallPill(g, hx, 6, p, headerH / 2);
      hx += 3;
    }

    // Superclass constraints
    let curY = headerH;
    if (superclasses.length > 0) {
      let sx = 12;
      g.appendChild(textEl(sx, curY + 11, '\u2190',
        `fill:${COLORS.constraintBd};font-size:11px;`));
      sx += CHAR_WIDTH + 4;
      for (let i = 0; i < superclasses.length; i++) {
        if (i > 0) {
          g.appendChild(textEl(sx, curY + 11, ',',
            `fill:${COLORS.separator};font-size:10px;`));
          sx += CHAR_WIDTH;
        }
        g.appendChild(textEl(sx, curY + 11, superclasses[i],
          `fill:${COLORS.constraint};font-weight:600;font-size:10px;`));
        sx += textWidth(superclasses[i]) + 4;
      }
      curY += superH;
    }

    // Methods
    if (parsedMethods.length === 0) {
      g.appendChild(textEl(16, curY + 20, '(no own methods)',
        `fill:#aaa;font-size:11px;font-style:italic;`));
    }

    for (let i = 0; i < parsedMethods.length; i++) {
      const m = parsedMethods[i];
      curY += 8;

      // Thin separator
      if (i > 0) {
        g.appendChild(svgEl('line', {
          x1: 8, y1: curY - 2,
          x2: totalW - 8, y2: curY - 2,
          stroke: '#e8e8f0', 'stroke-width': 1
        }));
      }

      if (m.ast) {
        const { forallVars, constraints, body } = unwrapType(m.ast);

        // Collect all vars for this method + class params
        const methodVars = [...typeParams, ...collectAllVars(m.ast)];
        _varColors = assignVarColors(methodVars);

        const mNameW = textWidth(m.name + ' :: ');

        // Constraints (inline, above method)
        if (constraints.length > 0) {
          let cx = 12 + mNameW;
          for (const c of constraints) {
            const ct = constraintText(c);
            const cw = textWidth(ct) + 12;
            g.appendChild(svgEl('rect', {
              x: cx, y: curY + 2, width: cw, height: 16,
              rx: 3, ry: 3,
              style: `fill:${COLORS.constraintBg};stroke:${COLORS.constraintBd};stroke-width:0.75;`
            }));
            g.appendChild(textEl(cx + 6, curY + 10, ct,
              `fill:${COLORS.constraint};font-weight:600;font-size:10px;`));
            cx += cw + 4;
          }
          curY += 20;
        }

        // Method name + body
        const bodyM = measure(body);
        const bodyH = Math.max(LINE_HEIGHT, bodyM.height);

        g.appendChild(textEl(12, curY + LINE_HEIGHT / 2, m.name,
          `fill:${COLORS.name};font-weight:700;font-size:12px;`));
        g.appendChild(textEl(12 + textWidth(m.name), curY + LINE_HEIGHT / 2, ' :: ',
          `fill:${COLORS.separator};font-size:12px;`));
        renderNode(g, 12 + mNameW, curY, body);

        curY += bodyH;

        // Forall underneath method name (consistent with main signatures)
        if (forallVars.length > 0) {
          curY += 3; // breathing room
          let fx = 12;
          g.appendChild(textEl(fx, curY + 8, FORALL,
            `fill:${COLORS.keyword};font-weight:700;font-size:13px;`));
          fx += CHAR_WIDTH * 1.2;
          for (let vi = 0; vi < forallVars.length; vi++) {
            if (vi > 0) fx += 2;
            fx += renderSmallPill(g, fx, curY, forallVars[vi], curY + 8);
          }
          curY += 18;
        }

        curY += 8;
      } else {
        g.appendChild(textEl(12, curY + 10, m.name + ' :: ' + m.sig,
          `fill:#999;font-size:11px;`));
        curY += 24;
      }
    }

    _varColors = {};
    return svg;
  }

  return { renderSignature, renderADT, renderClassDef, measure, typeToText };

})();
