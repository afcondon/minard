export const appendHtmlInto = (selector) => (html) => () => {
  const el = document.querySelector(selector);
  if (el) el.innerHTML += html;
};
