// ModuleSignatureMap FFI
//
// Category-lane layout of a module's type signatures.
// Each declaration cell is sized to its rendered SVG bounding box.
// No title bars — the SVG already contains the name for values/types,
// and data types use renderADT's graphical tree rendering.

// Kind → light background color
const kindBg = (kind) => {
  switch (kind) {
    case "value":        return "#f0f4fa";
    case "data":         return "#eef6ee";
    case "newtype":      return "#eef8f7";
    case "type_class":   return "#fdf5ed";
    case "type_synonym": return "#fdf9ec";
    case "foreign":      return "#fdf0f0";
    case "alias":        return "#f5f0f8";
    default:             return "#f5f5f5";
  }
};

// Kind → subtle border color
const kindBorder = (kind) => {
  switch (kind) {
    case "value":        return "#c8d6e8";
    case "data":         return "#c0dcc0";
    case "newtype":      return "#b8dcd8";
    case "type_class":   return "#e8d4b8";
    case "type_synonym": return "#e8e0b0";
    case "foreign":      return "#e0c0c0";
    case "alias":        return "#d0c0d8";
    default:             return "#ddd";
  }
};

// Kind → accent color for lane headers
const kindAccent = (kind) => {
  switch (kind) {
    case "value":        return "#4e79a7";
    case "data":         return "#59a14f";
    case "newtype":      return "#76b7b2";
    case "type_class":   return "#f28e2b";
    case "type_synonym": return "#edc948";
    case "foreign":      return "#e15759";
    case "alias":        return "#b07aa1";
    default:             return "#999";
  }
};

// Lane definitions: semantic grouping of declaration kinds
const LANES = [
  { key: "structural", label: "Data Types",    kinds: ["data", "newtype"] },
  { key: "classes",    label: "Classes",        kinds: ["type_class"] },
  { key: "types",      label: "Type Aliases",   kinds: ["type_synonym"] },
  { key: "values",     label: "Values",         kinds: ["value"] },
  { key: "foreign",    label: "Foreign",         kinds: ["foreign", "alias"] },
];

// Cell sizing constants
const CELL_PAD = 8;         // padding inside cell around SVG
const MIN_CELL_W = 120;     // minimum cell width
const MIN_CELL_H = 40;      // minimum cell height
const CELL_GAP = 6;         // gap between cells
const LANE_GAP = 16;        // gap between lanes

// Extract constructor argument types from a parsed constructor signature.
// Constructor sigs look like "a -> b -> MyType a b" — we want just the arg types.
function extractCtorArgs(sig, parseFn) {
  if (!sig) return [];
  const result = parseFn(sig);
  if (!result.ok) return sig ? [sig] : [];

  if (result.ast.tag === "function") {
    // Function type: params are the constructor args, returnType is the data type itself
    return result.ast.params.map(p => TypeSigRenderer.typeToText(p));
  }
  // Not a function type — zero-arg constructor (e.g., Nothing, True)
  return [];
}

// Collect all type variable names from constructor signatures
function inferTypeParams(children, parseFn) {
  const vars = new Set();
  for (const child of children) {
    if (!child.sig) continue;
    const result = parseFn(child.sig);
    if (!result.ok) continue;
    collectTypeVars(result.ast, vars);
  }
  return Array.from(vars);
}

function collectTypeVars(node, vars) {
  if (!node) return;
  switch (node.tag) {
    case "typevar": vars.add(node.name); break;
    case "function":
      for (const p of node.params) collectTypeVars(p, vars);
      collectTypeVars(node.returnType, vars);
      break;
    case "applied":
      collectTypeVars(node.constructor, vars);
      for (const a of (node.args || [])) collectTypeVars(a, vars);
      break;
    case "forall":
      // forall vars are bound, still collect from body
      collectTypeVars(node.body, vars);
      break;
    case "constrained":
      for (const c of (node.constraints || [])) {
        for (const a of (c.args || [])) collectTypeVars(a, vars);
      }
      collectTypeVars(node.body, vars);
      break;
    case "parens":
      collectTypeVars(node.inner, vars);
      break;
    case "record": case "row":
      for (const f of (node.fields || [])) collectTypeVars(f.type, vars);
      break;
    case "operator":
      collectTypeVars(node.left, vars);
      collectTypeVars(node.right, vars);
      break;
    case "kinded":
      collectTypeVars(node.type, vars);
      break;
  }
}

export const renderSignatureMapImpl = (containerSelector) => (moduleName) => (cells) => (parseFn) => () => {
  const container = document.querySelector(containerSelector);
  if (!container) return { cleanup: () => {} };

  container.innerHTML = '';
  container.style.cssText = 'overflow-y: auto; padding: 12px 16px;';

  const containerW = container.clientWidth || 1200;
  const maxCellW = containerW - 40;

  if (cells.length === 0) {
    container.innerHTML = '<div style="display:flex;align-items:center;justify-content:center;height:100%;color:#999;font-size:14px;">No declarations</div>';
    return { cleanup: () => { container.innerHTML = ''; } };
  }

  // Phase 1: Pre-render all SVGs, measure actual dimensions, compute cell sizes.
  const measured = cells.map(cell => {
    let svg = null;
    let svgW = 0, svgH = 0;

    if ((cell.kind === "data" || cell.kind === "newtype") && cell.children.length > 0) {
      // Data types: use renderADT for the tree rendering
      try {
        const typeParams = inferTypeParams(cell.children, parseFn);
        const constructors = cell.children
          .filter(c => c.kind === "constructor" || !c.kind || c.kind === "")
          .map(c => ({
            name: c.name,
            args: extractCtorArgs(c.sig, parseFn)
          }));
        svg = TypeSigRenderer.renderADT(cell.name, typeParams, constructors, parseFn);
        svgW = parseFloat(svg.getAttribute('width')) || 200;
        svgH = parseFloat(svg.getAttribute('height')) || 60;
      } catch (e) {
        svg = null;
      }
    } else if (cell.kind === "type_class" && cell.children.length > 0) {
      // Type classes: use renderClassDef
      try {
        const typeParams = []; // TODO: extract from class signature
        const superclasses = []; // TODO: extract from constraints
        const methods = cell.children
          .filter(c => c.kind === "class_member" || !c.kind || c.kind === "")
          .map(c => ({ name: c.name, sig: c.sig || "" }));
        svg = TypeSigRenderer.renderClassDef(cell.name, typeParams, superclasses, methods, parseFn);
        svgW = parseFloat(svg.getAttribute('width')) || 200;
        svgH = parseFloat(svg.getAttribute('height')) || 60;
      } catch (e) {
        svg = null;
      }
    } else if (cell.ast.ok) {
      // Values, type synonyms, foreign: use renderSignature
      try {
        svg = TypeSigRenderer.renderSignature(cell.name, cell.sig || '', cell.ast.ast, {});
        svgW = parseFloat(svg.getAttribute('width')) || 100;
        svgH = parseFloat(svg.getAttribute('height')) || 36;
      } catch (e) {
        svg = null;
      }
    }

    // Size cell to SVG bounding box + padding
    const cellWidth = svg
      ? Math.min(maxCellW, Math.max(MIN_CELL_W, svgW + CELL_PAD * 2))
      : MIN_CELL_W;
    const cellHeight = svg
      ? Math.max(MIN_CELL_H, svgH + CELL_PAD * 2)
      : MIN_CELL_H;

    return { ...cell, svg, svgW, svgH, cellWidth, cellHeight };
  });

  // Phase 2: Group into lanes and render
  for (const lane of LANES) {
    const laneCells = measured.filter(c => lane.kinds.includes(c.kind));
    if (laneCells.length === 0) continue;

    const accent = kindAccent(lane.kinds[0]);

    // Lane container
    const laneDiv = document.createElement('div');
    laneDiv.style.cssText = `margin-bottom: ${LANE_GAP}px;`;

    // Lane header
    const laneHeader = document.createElement('div');
    laneHeader.style.cssText = `
      display: flex; align-items: center; gap: 8px;
      padding: 4px 0; margin-bottom: 6px;
      border-bottom: 2px solid ${accent};
    `;

    const laneLabel = document.createElement('span');
    laneLabel.textContent = lane.label;
    laneLabel.style.cssText = `
      font-family: 'Courier New', Courier, monospace;
      font-size: 11px; font-weight: 700; color: ${accent};
      text-transform: uppercase; letter-spacing: 0.5px;
    `;
    laneHeader.appendChild(laneLabel);

    const laneCount = document.createElement('span');
    laneCount.textContent = laneCells.length;
    laneCount.style.cssText = `
      font-size: 9px; padding: 1px 5px; border-radius: 8px;
      background: ${accent}; color: white; font-weight: 600;
    `;
    laneHeader.appendChild(laneCount);
    laneDiv.appendChild(laneHeader);

    // Cells container: flex-wrap shelf packing
    const cellsDiv = document.createElement('div');
    cellsDiv.style.cssText = `
      display: flex; flex-wrap: wrap; gap: ${CELL_GAP}px;
      align-items: flex-start;
    `;

    for (const d of laneCells) {
      const div = document.createElement('div');
      div.style.cssText = `
        width: ${d.cellWidth}px;
        height: ${d.cellHeight}px;
        overflow: hidden;
        background: ${kindBg(d.kind)};
        border: 1px solid ${kindBorder(d.kind)};
        border-radius: 3px;
        cursor: pointer;
        transition: box-shadow 0.15s ease, transform 0.15s ease;
        flex-shrink: 0;
        position: relative;
      `;

      div.addEventListener('mouseenter', () => {
        div.style.boxShadow = '0 2px 8px rgba(0,0,0,0.15)';
        div.style.transform = 'translateY(-1px)';
        div.style.zIndex = '10';
      });
      div.addEventListener('mouseleave', () => {
        div.style.boxShadow = 'none';
        div.style.transform = 'none';
        div.style.zIndex = '';
      });

      if (d.onClick) {
        div.addEventListener('click', () => d.onClick());
      }

      if (d.svg) {
        // Insert pre-rendered SVG directly (no title bar)
        d.svg.style.position = 'absolute';
        d.svg.style.left = `${CELL_PAD}px`;
        d.svg.style.top = `${CELL_PAD}px`;
        d.svg.style.transformOrigin = 'top left';

        // Scale down only if SVG exceeds cell width (capped at maxCellW)
        const availW = d.cellWidth - CELL_PAD * 2;
        if (d.svgW > availW) {
          const scale = availW / d.svgW;
          d.svg.style.transform = `scale(${scale})`;
        }

        div.appendChild(d.svg);
      } else {
        // Fallback: plain text name + signature
        const fallback = document.createElement('div');
        fallback.style.cssText = `
          padding: ${CELL_PAD}px; font-size: 11px; color: #333;
          font-family: 'Fira Code', 'SF Mono', monospace;
        `;
        fallback.textContent = d.name + (d.sig ? ' :: ' + d.sig : '');
        div.appendChild(fallback);
      }

      cellsDiv.appendChild(div);
    }

    laneDiv.appendChild(cellsDiv);
    container.appendChild(laneDiv);
  }

  return { cleanup: () => { container.innerHTML = ''; } };
};
