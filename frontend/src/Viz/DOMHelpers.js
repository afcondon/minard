export const scrollElementIntoView = (elementId) => () => {
  const el = document.getElementById(elementId);
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });
};

export const scrollChildIntoView = (containerId) => (childSelector) => () => {
  requestAnimationFrame(() => {
    const container = document.getElementById(containerId);
    if (!container) return;
    const child = container.querySelector(childSelector);
    if (child) child.scrollIntoView({ behavior: 'instant', block: 'center' });
  });
};

export const setInnerHTML = (selector) => (html) => () => {
  const el = document.querySelector(selector);
  if (el) el.innerHTML = html;
};

export const setDocumentTitle = (title) => () => {
  document.title = title;
};
