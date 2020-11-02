/**
 * @typedef { { selected: boolean; visible: boolean } } FilterItem
 * @typedef { { fKeywords: Record<string, FilterItem> } } Filters
 */

class Filter {
  /**
   * @param value { string }
   * @param filters { Filters }
   * @param elementsRefs { Element[] }
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
      .map((elRef) => this._getDatasetWithKeywordData(elRef.dataset))
      .reduce((filtersObject, datasets) => {
        datasets.forEach(([key, value]) => {
          this._splitByComma(value).forEach((val) => {
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
    const newFilter = { selected: true, visible: true }

    const newFilters = this._elementsRefs.reduce((filtersObject, elementRef) => {
      this._getDatasetWithKeywordData(elementRef.dataset).forEach(([key, value]) =>
        this._splitByComma(value).forEach((val) => {
          filtersObject[key] = filtersObject[key] 
            ? { ...filtersObject[key], [val]: filtersObject[key][val] ?? newFilter}
            : { [val]: newFilter }
        })
      );
      return filtersObject;
    }, {});

    const shouldAddDefaultFilter = this._elementsRefs.some(ref => !!ref.dataset['fKeywords'])

    return shouldAddDefaultFilter 
      ? { ...newFilters, fKeywords: { ...newFilters.fKeywords, default: newFilter } } 
      : newFilters
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
  * @returns { [key: string, value: string][] }
  */
  _getDatasetWithKeywordData = (dataset) =>
    Object.entries(dataset).filter(([key]) => startsWith(key, "f"));
}
