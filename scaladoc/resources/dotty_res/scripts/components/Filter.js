/**
 * @typedef { Record<string, FilterItem> } FilterMap
 * @typedef { "fKeywords" | "fInherited" | "fImplicitly" | "fExtension" | "fVisibility" } FilterAttributes
 * @typedef { Record<FilterAttributes, FilterMap> } Filters
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

  static get defaultFilters() {
    return scaladocData.filterDefaults
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
    const elementsDatasets = this.elementsRefs
      .filter(element => {
        const name = getElementTextContent(getElementNameRef(element));
        const description = getElementTextContent(getElementDescription(element));

        return name.includes(value) || description.includes(value);
      })
      .map(element => this._getDatasetWithKeywordData(element.dataset))

      const newFilters = elementsDatasets.reduce((filtersObject, datasets) => {
        datasets.forEach(([key, value]) => {
          this._splitByComma(value).forEach((val) => {
            filtersObject[key] = { ...filtersObject[key], [val]: { ...filtersObject[key][val], visible: true} };
          });
        });

        return filtersObject;
      }, this._allFiltersAreHidden());

      return this._attachDefaultFilters(newFilters)

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
    const newFilters = this._elementsRefs.reduce((filtersObject, elementRef) => {
      this._getDatasetWithKeywordData(elementRef.dataset).forEach(([key, value]) =>
        this._splitByComma(value).forEach((val) => {
          filtersObject[key] = filtersObject[key]
            ? { ...filtersObject[key], [val]: filtersObject[key][val] ?? new FilterItem() }
            : { [val]: new FilterItem()  }
        })
      );
      return filtersObject;
    }, {});

    return this._attachDefaultFilters(newFilters)
  }

  /**
   * @private
   * @param {Filters} newFilters
   * @returns {Filters}
   */
  _attachDefaultFilters(newFilters) {
    return Object.entries(Filter.defaultFilters).reduce((acc, [key, defaultFilter]) => {
      const filterKey = getFilterKey(key)
      const shouldAddDefaultKeywordFilter = this._elementsRefs.some(ref => !!ref.dataset[filterKey])

      return shouldAddDefaultKeywordFilter
        ? {
          ...acc,
          [filterKey]: {
            ...acc[filterKey],
            [defaultFilter]: new FilterItem()
          }
        }
        : acc
    }, newFilters)
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
    Object.entries(dataset).filter(([key]) => isFilterData(key));
}

class FilterItem {
  constructor(selected = true, visible = true) {
    this.selected = selected
    this.visible = visible
  }
}