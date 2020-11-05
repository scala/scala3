import { Component } from '../common/Component';
import {
  findRefs,
  findRef,
  getElementTextContent,
  getElementNameRef,
  getElementDescription,
  getFilterKey,
  isFilterData,
  Dataset,
  FilterAttributes,
} from '../common/util';
import { Filter } from './Filter';

type Props = { filter: Filter };
type State = { list: List };
type ListElement = { ref: HTMLElement; name: string | null; description: string | null };

export class DocumentableList extends Component<Props, State> {
  private refs: Record<'tabs' | 'sections', HTMLElement[]>;

  constructor(props: Props) {
    super(props);

    this.refs = {
      tabs: findRefs('.section-tab[data-togglable]', findRef('.tabbedcontent')),
      sections: findRefs('div[data-togglable]', findRef('.tabbedcontent')),
    };

    this.state = {
      list: new List(this.refs.tabs, this.refs.sections),
    };

    this.render(props);
  }

  toggleElementDatasetVisibility(isVisible: boolean | undefined, ref: HTMLElement) {
    ref.dataset.visibility = isVisible?.toLocaleString();
  }

  toggleDisplayStyles(
    condition: boolean | undefined,
    ref: HTMLElement | undefined,
    onVisibleStyle: string,
  ) {
    ref && (ref.style.display = condition ? onVisibleStyle : 'none');
  }

  render({ filter }: Props) {
    this.state.list.sectionsRefs.map(sectionRef => {
      const tabRef = this.state.list.getTabRefFromSectionRef(sectionRef);

      const isTabVisible = !!this.state.list.getSectionListRefs(sectionRef).filter(listRef => {
        const isListVisible = !!this.state.list
          .getSectionListElementsRefs(listRef)
          .map(elementRef => this.state.list.toListElement(elementRef))
          .filter(elementData => {
            const isElementVisible = this.state.list.isElementVisible(elementData, filter);

            this.toggleDisplayStyles(isElementVisible, elementData.ref, 'table');
            this.toggleElementDatasetVisibility(isElementVisible, elementData.ref);

            return isElementVisible;
          }).length;

        this.toggleDisplayStyles(isListVisible, listRef, 'block');

        return isListVisible;
      }).length;

      this.toggleDisplayStyles(isTabVisible, tabRef, 'inline-block');
    });
  }
}

class List {
  constructor(private tabsRef: HTMLElement[], private sectionRefs: HTMLElement[]) {}

  get tabsRefs() {
    return this.tabsRef.filter(tabRef => this.filterTab(this.getTogglable(tabRef)));
  }

  get sectionsRefs() {
    return this.sectionRefs.filter(sectionRef => this.filterTab(this.getTogglable(sectionRef)));
  }

  filterTab(name: string) {
    return name !== 'Linear supertypes' && name !== 'Known subtypes' && name !== 'Type hierarchy';
  }

  getTabRefFromSectionRef(sectionRef: HTMLElement) {
    return this.tabsRefs.find(
      tabRef => this.getTogglable(tabRef) === this.getTogglable(sectionRef),
    );
  }

  getSectionListRefs(sectionRef: HTMLElement) {
    return findRefs('.documentableList', sectionRef);
  }

  getSectionListElementsRefs(listRef: HTMLElement) {
    return findRefs('.documentableElement', listRef);
  }

  toListElement(elementRef: HTMLElement) {
    return {
      ref: elementRef,
      name: getElementTextContent(getElementNameRef(elementRef)),
      description: getElementTextContent(getElementDescription(elementRef)),
    };
  }

  isElementVisible(elementData: ListElement, filter: Filter) {
    return !areFiltersFromElementSelected() ? false : includesInputValue();

    function includesInputValue() {
      return (
        elementData.name?.includes(filter.value) || elementData.description?.includes(filter.value)
      );
    }

    function areFiltersFromElementSelected() {
      const dataset = Object.entries(elementData.ref.dataset) as Dataset;
      const defaultFilters = Object.entries(Filter.defaultFilters).filter(
        ([key]) => !!filter.filters[getFilterKey(key)],
      ) as Dataset;

      const defaultFiltersForMembersWithoutDataAttribute = defaultFilters.reduce<Dataset>(
        (acc, [key, value]) => {
          const filterKey = getFilterKey(key);
          const shouldAddDefaultFilter = !dataset.some(([k]) => k === filterKey);
          return shouldAddDefaultFilter ? [...acc, [filterKey, value]] : acc;
        },
        [],
      );

      const datasetWithAppendedDefaultFilters: Dataset = dataset
        .filter(([k]) => isFilterData(k))
        .map(([k, v]) => {
          const defaultFilter = defaultFilters.find(([defaultKey]) => defaultKey === k);
          return defaultFilter ? [k, `${v},${defaultFilter[1]}`] : [k, v];
        });

      const datasetWithDefaultFilters = [
        ...defaultFiltersForMembersWithoutDataAttribute,
        ...datasetWithAppendedDefaultFilters,
      ];

      const isVisible = datasetWithDefaultFilters.every(([filterKey, value]) => {
        const filterGroup = filter.filters[filterKey as FilterAttributes];

        return value.split(',').some(v => filterGroup && filterGroup[v].selected);
      });

      return isVisible;
    }
  }

  private getTogglable = (elementData: HTMLElement) => elementData.dataset.togglable!;
}
