// FFI for CE2.SigTest.Main

// Create a 3-column table row: index, metadata, render container
export const createRow = (parentId) => (idx) => (name) => (category) => (sigText) => (containerId) => (parseOk) => () => {
  const table = document.getElementById(parentId);
  if (!table) return;

  const tr = document.createElement('tr');
  tr.className = parseOk ? 'sig-row' : 'sig-row sig-row-error';

  // Index cell
  const tdIdx = document.createElement('td');
  tdIdx.className = 'sig-idx';
  tdIdx.textContent = String(idx);
  tr.appendChild(tdIdx);

  // Meta cell: name + category + raw sig
  const tdMeta = document.createElement('td');
  tdMeta.className = 'sig-meta';
  const nameEl = document.createElement('div');
  nameEl.className = 'sig-meta-name';
  nameEl.textContent = name;
  const catEl = document.createElement('div');
  catEl.className = 'sig-meta-cat';
  catEl.textContent = category;
  const rawEl = document.createElement('div');
  rawEl.className = 'sig-meta-raw';
  rawEl.textContent = sigText;
  tdMeta.appendChild(nameEl);
  tdMeta.appendChild(catEl);
  tdMeta.appendChild(rawEl);
  tr.appendChild(tdMeta);

  // Render cell
  const tdRender = document.createElement('td');
  tdRender.className = 'sig-render-cell';
  const renderDiv = document.createElement('div');
  renderDiv.id = containerId;
  tdRender.appendChild(renderDiv);
  tr.appendChild(tdRender);

  table.appendChild(tr);
};
