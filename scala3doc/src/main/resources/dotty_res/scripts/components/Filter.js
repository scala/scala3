const defaultFilterGroup = {
  FOrdering: { Alphabetical: true },
};

class Filter {
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

  onFilterToggle(key, value) {
    return new Filter(
      this.value,
      this._withToggledFilter(key, value),
      this.elementsRefs
    );
  }

  onGroupSelectionChange(key, isActive) {
    return new Filter(
      this.value,
      this._withNewSelectionOfGroup(key, isActive),
      this.elementsRefs
    );
  }

  onInputValueChange(value) {
    return new Filter(
      value,
      this._generateFiltersOnTyping(value),
      this.elementsRefs
    );
  }

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

  _withNewFilters() {
    return this._elementsRefs.reduce((filtersObject, elementRef) => {
      this._getDatasetWithF(elementRef.dataset).map(([key, value]) =>
        this._splitByComma(value).map((val) => {
          if (!filtersObject[key]) {
            filtersObject[key] = { [val]: { selected: true, visible: true } };
          } else {
            filtersObject[key] = {
              ...filtersObject[key],
              [val]: filtersObject[key][val] ?? {
                selected: true,
                visible: true,
              },
            };
          }
        })
      );
      return filtersObject;
    }, {});
  }

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

  _splitByComma = (str) => str.split(",");

  _getDatasetWithF = (dataset) =>
    Object.entries(dataset).filter(([key]) => this._startsWithF(key));

  _startsWithF = (str) => startsWith(str, "f");
}
