export const scrollElementIntoView = (elementId) => () => {
  const el = document.getElementById(elementId);
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });
};

export const setInnerHTML = (selector) => (html) => () => {
  const el = document.querySelector(selector);
  if (el) el.innerHTML = html;
};
