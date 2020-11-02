class FilterGroup extends Component {
  constructor(props) {
    super(props);

    this.filterToggleRef = findRef(".filterToggleButton");
    this.filterLowerContainerRef = findRef(".filterLowerContainer");

    withEvent(
      this.filterToggleRef,
      "click",
      this.props.onFilterVisibilityChange
    );

    this.render(this.props);
  }

  onFilterClick = ({
    currentTarget: {
      dataset: { key, value },
    },
  }) => {
    this.props.onFilterToggle(key, value);
  };

  onSelectAllClick = ({
    currentTarget: {
      dataset: { key },
    },
  }) => {
    this.props.onGroupSelectChange(key, true);
  };

  onDeselectAllClick = ({
    currentTarget: {
      dataset: { key },
    },
  }) => {
    this.props.onGroupSelectChange(key, false);
  };

  attachFiltersClicks() {
    const refs = findRefs(
      "button.filterButtonItem",
      this.filterLowerContainerRef
    );
    attachListeners(refs, "click", this.onFilterClick);
  }

  attachSelectingButtonsClicks() {
    const selectAllRefs = findRefs(
      "button.selectAll",
      this.filterLowerContainerRef
    );
    const deselectAllRefs = findRefs(
      "button.deselectAll",
      this.filterLowerContainerRef
    );

    attachListeners(selectAllRefs, "click", this.onSelectAllClick);
    attachListeners(deselectAllRefs, "click", this.onDeselectAllClick);
  }

  isActive(isActive) {
    return isActive ? "active" : "";
  }

  isVisible(visible) {
    return visible ? "visible" : "";
  }

  getSortedValues(values) {
    return Object.entries(values).sort((a, b) => a[0].localeCompare(b[0]));
  }

  getFilterGroup(title, values) {
    return `
      <div class="filterGroup">
        <div class="groupTitle">
          <span>${title.substring(1)}</span>
          <div class="groupButtonsContainer">
            <button class="selectAll" data-key="${title}">Select All</button>
            <button class="deselectAll" data-key="${title}">Deselect All</button>
          </div>
        </div>
        <div class="filterList">
          ${this.getSortedValues(values)
            .map(
              ([key, data]) =>
                `<button class="filterButtonItem ${this.isActive(
                  data.selected
                )} ${this.isVisible(
                  data.visible
                )}" data-key="${title}" data-value="${key}">${key}</button>`
            )
            .join(" ")}
        </div>
      </div>
    `;
  }

  render({ filter }) {
    console.log(filter.filters)
    attachDOM(
      this.filterLowerContainerRef,
      Object.entries(filter.filters)
        .filter(([key, values]) => Object.values(values).some((v) => v.visible))
        .map(([key, values]) => this.getFilterGroup(key, values))
    );

    this.attachFiltersClicks();
    this.attachSelectingButtonsClicks();
  }
}
