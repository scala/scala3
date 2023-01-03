const findRef = (searchBy, element = document) =>
  element.querySelector(searchBy);

const findRefs = (searchBy, element = document) =>
  element ? [...element.querySelectorAll(searchBy)] : [];

const withEvent = (element, listener, callback) => {
  element && element.addEventListener(listener, callback);
  return () => element && element.removeEventListener(listener, callback);
};

const attachDOM = (element, html) => {
  if (element) {
    element.innerHTML = htmlToString(html);
  }
};

const htmlToString = (html) => {
  if (Array.isArray(html)) {
    return html.join("");
  }
  return html;
};

const isFilterData = key => key.startsWith("f")

const getFilterKey = key => `f${key.charAt(0).toUpperCase()}${key.slice(1)}`

const attachListeners = (elementsRefs, type, callback) =>
  elementsRefs.map((elRef) => withEvent(elRef, type, callback));

const getElementTextContent = (element) => (element ? element.textContent : "");

const getElementDescription = (elementRef) =>
  findRef(".documentableBrief", elementRef);

const getElementNameRef = (elementRef) =>
  findRef(".documentableName", elementRef);
