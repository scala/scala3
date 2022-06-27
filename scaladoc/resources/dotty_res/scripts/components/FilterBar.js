/**
 * @typedef { import("./Filter").Filter } Filter
 */

 class FilterBar extends Component {
  constructor(props) {
    super(props);

    this.refs = {
      elements: findRefs(".documentableElement"),
      filterBar: findRef(".documentableFilter"),
    };

    this.state = {
      filter: new Filter("", {}, this.refs.elements, true),
      isVisible: false,
      selectedPill: '',
    };

    this.inputComp = new Input({ onInputChange: this.onInputChange });
    this.listComp = new DocumentableList({
      filter: this.state.filter,
    });
    this.filterGroupComp = new FilterGroup({
      filter: this.state.filter,
      onFilterToggle: this.onFilterToggle,
      onGroupSelectChange: this.onGroupSelectChange,
      onFilterVisibilityChange: this.onFilterVisibilityChange,
      onPillClick: this.onPillClick,
      onPillCollapse: this.onPillCollapse,
    });

    this.render();
  }

  onInputChange = (value) => {
    this.setState((prevState) => ({
      filter: prevState.filter.onInputValueChange(value),
    }));
  };

  onGroupSelectChange = (key, isActive) => {
    this.setState((prevState) => ({
      filter: prevState.filter.onGroupSelectionChange(key, isActive),
    }));
  };

  onClearFilters = () => {
    this.setState(() => ({
      filter: ""
    }))
  }

  onFilterVisibilityChange = () => {
    this.setState((prevState) => ({ isVisible: !prevState.isVisible }));
  };

  onFilterToggle = (key, value) => {
    this.setState((prevState) => ({
      filter: prevState.filter.onFilterToggle(key, value),
    }));
  };

  onPillClick = (key) => {
    this.setState((prevState) => ({
      filter: prevState.filter,
      selectedPill: key
    }))
  }

  onPillCollapse = () => {
    this.setState((prevState) => ({
      filter: prevState.filter,
      selectedPill: ""
    }))
  }

  render() {
    if (this.refs.filterBar) {
      if (this.state.isVisible) {
        this.refs.filterBar.classList.add("active");
      } else {
        this.refs.filterBar.classList.remove("active");
      }
    }

    this.listComp.render({ filter: this.state.filter });
    this.filterGroupComp.render({ filter: this.state.filter, selectedPill: this.state.selectedPill });
  }
}

init(() => new FilterBar());
