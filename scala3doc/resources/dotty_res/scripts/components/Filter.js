const defaultFilterGroup = {
  FOrdering: { Alphabetical: true },
};

/**
 * @typedef { Object } FilterItem
 * @prop { string } selected
 * @prop { number } visible
 */

 /**
 * @typedef { Record<string, FilterItem> } Filters
 */

class Filter {
  /**
   * @param value { string }
   * @param filters { Filters }
   * @param elementsRefs { Element }
   */
  constructor(value, filters, elementsRefs, init = false) {
    this._init = init;
    this._value = value;
    this._elementsRefs = elementsRefs;

    this._filters = this._init ? this._withNewFilters() : filters;
  }

  get value() {
    return this._value;
  }

  get filters() {
    return this._filters;
  }

  get elementsRefs() {
    return this._elementsRefs;
  }

  /**
  * @param key { string }
  * @param value { string }
  */
  onFilterToggle(key, value) {
    return new Filter(
      this.value,
      this._withToggledFilter(key, value),
      this.elementsRefs
    );
  }

  /**
  * @param key { string }
  * @param isActive { boolean }
  */
  onGroupSelectionChange(key, isActive) {
    return new Filter(
      this.value,
      this._withNewSelectionOfGroup(key, isActive),
      this.elementsRefs
    );
  }

  /**
  * @param value { string }
  * @returns { FilterItem }
  */
  onInputValueChange(value) {
    return new Filter(
      value,
      this._generateFiltersOnTyping(value),
      this.elementsRefs
    );
  }

  /**
  * @private
  * @param value { string }
  * @returns { Filters }
  */
  _generateFiltersOnTyping(value) {
    return this.elementsRefs
      .filter((elRef) => {
        const name = getElementTextContent(getElementNameRef(elRef));
        const description = getElementTextContent(getElementDescription(elRef));

        return name.includes(value) || description.includes(value);
      })
      .map((elRef) => this._getDatasetWithF(elRef.dataset))
      .reduce((filtersObject, datasets) => {
        datasets.map(([key, value]) => {
          this._splitByComma(value).map((val) => {
            filtersObject[key] = {
              ...filtersObject[key],
              [val]: {
                ...filtersObject[key][val],
                visible: true,
              },
            };
          });
        });
        return filtersObject;
      }, this._allFiltersAreHidden());
  }

  /**
  * @private
  * @returns { Filters }
  */
  _allFiltersAreHidden() {
    return Object.entries(this.filters).reduce(
      (filters, [key, filterGroup]) => {
        filters[key] = Object.keys(filterGroup).reduce(
          (group, key) => (
            (group[key] = { ...filterGroup[key], visible: false }), group
          ),
          {}
        );
        return filters;
      },
      {}
    );
  }

  /**
  * @private
  * @param key { string }
  * @param isActive { boolean }
  * @returns { Filters }
  */
  _withNewSelectionOfGroup(key, isActive) {
    return {
      ...this.filters,
      [key]: Object.keys(this.filters[key]).reduce(
        (obj, filterKey) => (
          (obj[filterKey] = {
            ...this.filters[key][filterKey],
            ...(this.filters[key][filterKey].visible && { selected: isActive }),
          }),
          obj
        ),
        {}
      ),
    };
  }

 /**
  * @private
  * @returns { Filters }
  */
  _withNewFilters() {
    console.log("this._elementsRefs", this._elementsRefs)
    const newFilters = this._elementsRefs.reduce((filtersObject, elementRef) => {
      this._getDatasetWithF(elementRef.dataset).map(([key, value]) =>
        this._splitByComma(value).map((val) => {
          if (!filtersObject[key]) {
            filtersObject[key] = { [val]: { selected: true, visible: true } };
          } else {
            filtersObject[key] = {
              ...filtersObject[key],
              [val]: filtersObject[key][val] ?? { selected: true, visible: true },
            };
          }
        })
      );
      return filtersObject;
    }, {});
    console.log("newFilters", newFilters)
    return newFilters
  }

  /**
  * @private
  * @param key { string }
  * @param value { string }
  * @returns { Filters }
  */
  _withToggledFilter(key, value) {
    return {
      ...this.filters,
      [key]: {
        ...this.filters[key],
        [value]: {
          ...this.filters[key][value],
          selected: !this.filters[key][value].selected,
        },
      },
    };
  }

  /**
  * @private
  * @param str { string }
  */
  _splitByComma = (str) => str.split(",");

  /**
  * @private
  * @param dataset { DOMStringMap }
  */
  _getDatasetWithF = (dataset) =>
    Object.entries(dataset).filter(([key]) => startsWith(key, "f"));
}
