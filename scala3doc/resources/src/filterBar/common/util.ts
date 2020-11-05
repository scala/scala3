export const findRef = (searchBy: string, element: HTMLElement | Document = document) =>
  (element.querySelector(searchBy) as HTMLElement) ?? undefined;

export const findRefs = (
  searchBy: string,
  element: HTMLElement | Document = document,
): HTMLElement[] => (element ? Array.from(element.querySelectorAll(searchBy)) : []);

export const withEvent = (
  element: HTMLElement | null,
  listener: string,
  callback: (e: Event) => void,
) => {
  element && element.addEventListener(listener, callback);
  return () => element && element.removeEventListener(listener, callback);
};

type HTML = string | string[];
export type Dataset = [key: string, value: string][];

export const attachDOM = (element: HTMLElement | null, html: HTML) => {
  if (element) {
    element.innerHTML = htmlToString(html);
  }
};

export const htmlToString = (html: string | string[]) => {
  if (Array.isArray(html)) {
    return html.join('');
  }
  return html;
};

export type FilterAttributes =
  | 'fKeywords'
  | 'fInherited'
  | 'fImplicitly'
  | 'fExtension'
  | 'fVisibility';

export const isFilterData = (key: string): key is FilterAttributes => key.startsWith('f');
export const getFilterKey = (key: string): FilterAttributes =>
  `f${key.charAt(0).toUpperCase()}${key.slice(1)}` as FilterAttributes;

export const attachListeners = (
  elementsRefs: HTMLElement[],
  type: string,
  callback: (e: Event) => void,
) => elementsRefs.map(elRef => withEvent(elRef, type, callback));

export const getElementTextContent = (element?: HTMLElement | null) =>
  element ? element.textContent : '';

export const getElementDescription = (element: HTMLElement) =>
  findRef('.documentableBrief', element);

export const getElementNameRef = (element: HTMLElement) => findRef('.documentableName', element);
