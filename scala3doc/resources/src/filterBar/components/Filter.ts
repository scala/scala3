import {
  getElementTextContent,
  getElementNameRef,
  getElementDescription,
  getFilterKey,
  isFilterData,
  FilterAttributes,
} from '../common/util';

export type FilterMap = Record<string, FilterItem>;
export type Filters = Record<FilterAttributes, FilterMap>;

export class Filter {
  public filters: Filters;

  constructor(
    public value: string,
    filters: Filters,
    private elementsRefs: HTMLElement[],
    private init = false,
  ) {
    this.filters = this.init ? this.withNewFilters() : filters;
  }

  static get defaultFilters() {
    // global object injected into HTML by scala3doc
    // @ts-ignore
    return scala3DocData.filterDefaults;
  }

  onFilterToggle(key: FilterAttributes, value: string) {
    return new Filter(this.value, this.withToggledFilter(key, value), this.elementsRefs);
  }

  onGroupSelectionChange(key: FilterAttributes, isActive: boolean) {
    return new Filter(this.value, this.withNewSelectionOfGroup(key, isActive), this.elementsRefs);
  }

  onInputValueChange(value: string) {
    return new Filter(value, this.generateFiltersOnTyping(value), this.elementsRefs);
  }

  private generateFiltersOnTyping(value: string): Filters {
    const elementsDatasets = this.elementsRefs
      .filter(element => {
        const name = getElementTextContent(getElementNameRef(element));
        const description = getElementTextContent(getElementDescription(element));

        return name?.includes(value) || description?.includes(value);
      })
      .map(element => this.getDatasetWithKeywordData(element.dataset));

    const newFilters = elementsDatasets.reduce((filtersObject, datasets) => {
      datasets.forEach(([key, value]) => {
        const filterKey = key as FilterAttributes;
        this.splitByComma(value).forEach(val => {
          filtersObject[filterKey] = {
            ...filtersObject[filterKey],
            [val]: { ...filtersObject[filterKey][val], visible: true },
          };
        });
      });

      return filtersObject;
    }, this.allFiltersAreHidden());

    return this.attachDefaultFilters(newFilters);
  }

  private allFiltersAreHidden(): Filters {
    return Object.entries(this.filters).reduce<Filters>((filters, [key, filterGroup]) => {
      filters[key as FilterAttributes] = Object.keys(filterGroup).reduce<FilterMap>(
        (group, key) => ((group[key] = { ...filterGroup[key], visible: false }), group),
        {},
      );
      return filters;
    }, {} as Filters);
  }

  private withNewSelectionOfGroup(key: FilterAttributes, isActive: boolean): Filters {
    return {
      ...this.filters,
      [key]: Object.keys(this.filters[key]).reduce<FilterMap>(
        (obj, filterKey) => (
          (obj[filterKey] = {
            ...this.filters[key][filterKey],
            ...(this.filters[key][filterKey].visible && { selected: isActive }),
          }),
          obj
        ),
        {} as FilterMap,
      ),
    };
  }

  private withNewFilters(): Filters {
    const newFilters = this.elementsRefs.reduce<Filters>((filtersObject, elementRef) => {
      this.getDatasetWithKeywordData(elementRef.dataset).forEach(([key, value]) =>
        this.splitByComma(value).forEach(val => {
          const filterKey = key as FilterAttributes;
          filtersObject[filterKey] = filtersObject[filterKey]
            ? {
                ...filtersObject[filterKey],
                [val]: filtersObject[filterKey][val] ?? new FilterItem(),
              }
            : { [val]: new FilterItem() };
        }),
      );
      return filtersObject;
    }, {} as Filters);

    return this.attachDefaultFilters(newFilters);
  }

  private attachDefaultFilters(newFilters: Filters): Filters {
    return Object.entries(Filter.defaultFilters).reduce((acc, [key, defaultFilter]) => {
      const filterKey = getFilterKey(key);
      const shouldAddDefaultKeywordFilter = this.elementsRefs.some(ref => !!ref.dataset[filterKey]);

      return shouldAddDefaultKeywordFilter
        ? {
            ...acc,
            [filterKey]: {
              ...acc[filterKey],
              [defaultFilter as string]: new FilterItem(),
            },
          }
        : acc;
    }, newFilters);
  }

  private withToggledFilter(key: FilterAttributes, value: string): Filters {
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

  private splitByComma = (str: string) => str.split(',');

  private getDatasetWithKeywordData = (dataset: DOMStringMap): [key: string, value: string][] =>
    Object.entries(dataset).filter(([key]) => isFilterData(key)) as [key: string, value: string][];
}

class FilterItem {
  constructor(public selected = true, public visible = true) {}
}
