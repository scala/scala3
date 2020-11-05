/**
 * @typedef { import("./Filter").Filter } Filter
 */

import { Component } from '../common/Component';
import { findRefs, findRef, FilterAttributes } from '../common/util';
import { Filter, Filters } from './Filter';
import { DocumentableList } from './DocumentableList';
import { FilterGroup } from './FilterGroup';
import { Input } from './Input';

type Props = void;

type State = {
  filter: Filter;
  isVisible: boolean;
};

export class FilterBar extends Component<Props, State> {
  private refs: { elements: HTMLElement[]; filterBar: HTMLElement };
  private input: Input;
  private list: DocumentableList;
  private filterGroup: FilterGroup;

  static tryAttaching() {
    if (!findRef('.documentableFilter')) {
      return;
    }

    new FilterBar();
  }

  constructor(props: Props) {
    super(props);

    this.refs = {
      elements: findRefs('.documentableElement'),
      filterBar: findRef('.documentableFilter'),
    };

    this.state = {
      filter: new Filter('', {} as Filters, this.refs.elements, true),
      isVisible: false,
    };

    this.input = new Input({ onInputChange: this.onInputChange });
    this.list = new DocumentableList({
      filter: this.state.filter,
    });
    this.filterGroup = new FilterGroup({
      filter: this.state.filter,
      onFilterToggle: this.onFilterToggle,
      onGroupSelectChange: this.onGroupSelectChange,
      onFilterVisibilityChange: this.onFilterVisibilityChange,
    });

    this.render();
  }

  onInputChange = (value: string | null) => {
    this.setState(prevState => ({
      filter: prevState.filter.onInputValueChange(value!),
    }));
  };

  onGroupSelectChange = (key: string | undefined, isActive: boolean) => {
    this.setState(prevState => ({
      filter: prevState.filter.onGroupSelectionChange(key as FilterAttributes, isActive),
    }));
  };

  onFilterVisibilityChange = () => {
    this.setState(prevState => ({ isVisible: !prevState.isVisible }));
  };

  onFilterToggle = (key: string | undefined, value: string | undefined) => {
    this.setState(prevState => ({
      filter: prevState.filter.onFilterToggle(key as FilterAttributes, value!),
    }));
  };

  render() {
    if (this.refs.filterBar) {
      if (this.state.isVisible) {
        this.refs.filterBar.classList.add('active');
      } else {
        this.refs.filterBar.classList.remove('active');
      }
    }

    this.list.render({ filter: this.state.filter });
    this.filterGroup.render({
      filter: this.state.filter,
      onFilterToggle: this.onFilterToggle,
      onGroupSelectChange: this.onGroupSelectChange,
      onFilterVisibilityChange: this.onFilterVisibilityChange,
    });
  }
}
