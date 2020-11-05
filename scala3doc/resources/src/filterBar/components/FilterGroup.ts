import { Component } from '../common/Component';
import { findRef, findRefs, attachListeners, FilterAttributes, attachDOM } from '../common/util';
import { Filter, FilterMap } from './Filter';

type Props = {
  filter: Filter;
  onFilterVisibilityChange: (e: Event) => void;
  onFilterToggle: (key: string | undefined, value: string | undefined) => void;
  onGroupSelectChange: (key: string | undefined, value: boolean) => void;
};

export class FilterGroup extends Component<Props> {
  private filterToggleRef: HTMLElement;
  private filterLowerContainerRef: HTMLElement;

  constructor(props: Props) {
    super(props);

    this.filterToggleRef = findRef('.filterToggleButton');
    this.filterLowerContainerRef = findRef('.filterLowerContainer');

    this.filterToggleRef.addEventListener('click', e => this.props.onFilterVisibilityChange(e));
    this.render(this.props);
  }

  onFilterClick = ({ currentTarget }: Event) => {
    const { key, value } = (currentTarget as HTMLElement).dataset;
    this.props.onFilterToggle(key, value);
  };

  onSelectAllClick = ({ currentTarget }: Event) => {
    this.props.onGroupSelectChange((currentTarget as HTMLElement).dataset.key, true);
  };

  onDeselectAllClick = ({ currentTarget }: Event) => {
    this.props.onGroupSelectChange((currentTarget as HTMLElement).dataset.key, false);
  };

  private attachFiltersClicks() {
    const refs = findRefs('button.filterButtonItem', this.filterLowerContainerRef);
    attachListeners(refs, 'click', this.onFilterClick);
  }

  private attachSelectingButtonsClicks() {
    const selectAllRefs = findRefs('button.selectAll', this.filterLowerContainerRef);
    const deselectAllRefs = findRefs('button.deselectAll', this.filterLowerContainerRef);

    attachListeners(selectAllRefs, 'click', this.onSelectAllClick);
    attachListeners(deselectAllRefs, 'click', this.onDeselectAllClick);
  }

  isActive(isActive: boolean) {
    return isActive ? 'active' : '';
  }

  isVisible(visible: boolean) {
    return visible ? 'visible' : '';
  }

  getSortedValues(filterKey: FilterAttributes, values: FilterMap) {
    const defaultFilterKey = `${filterKey.charAt(1).toLowerCase()}${filterKey.slice(2)}`;
    const defaultGroupFilter = Filter.defaultFilters[defaultFilterKey];

    return Object.entries(values).sort(([a], [b]) => {
      if (a === defaultGroupFilter) {
        return -1;
      }

      if (b === defaultGroupFilter) {
        return 1;
      }

      return a.localeCompare(b);
    });
  }

  getFilterGroup(filterKey: FilterAttributes, values: FilterMap) {
    return `
      <div class="filterGroup" data-test-id="filterGroup">
        <div class="groupTitle">
          <span data-test-id="filterGroupTitle">${filterKey.substring(1)}</span>
          <div class="groupButtonsContainer" data-test-id="filterGroupBatchToggle">
            <button class="selectAll" data-key="${filterKey}">Select All</button>
            <button class="deselectAll" data-key="${filterKey}">Deselect All</button>
          </div>
        </div>
        <div class="filterList" data-test-id="filterGroupList">
          ${this.getSortedValues(filterKey, values)
            .map(
              ([key, data]) =>
                `<button class="filterButtonItem ${this.isActive(data.selected)} ${this.isVisible(
                  data.visible,
                )}" data-key="${filterKey}" data-selected="${
                  data.selected
                }" data-value="${key}" data-test-id="filterGroupButton">${key}</button>`,
            )
            .join(' ')}
        </div>
      </div>
    `;
  }

  render({ filter }: Props) {
    attachDOM(
      this.filterLowerContainerRef,
      Object.entries(filter.filters)
        .filter(([_key, values]) => Object.values(values).some(v => v.visible))
        .map(([key, values]) => this.getFilterGroup(key as FilterAttributes, values)),
    );

    this.attachFiltersClicks();
    this.attachSelectingButtonsClicks();
  }
}
