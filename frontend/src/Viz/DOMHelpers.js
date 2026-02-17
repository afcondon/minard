export const scrollElementIntoView = (elementId) => () => {
  const el = document.getElementById(elementId);
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });
};
