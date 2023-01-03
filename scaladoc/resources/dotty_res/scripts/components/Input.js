class Input extends Component {
  constructor(props) {
    super(props);

    this.inputRef = findRef(".filterableInput");
    this.onChangeFn = withEvent(this.inputRef, "input", this.onInputChange);
    this.onKeydownFn = withEvent(this.inputRef, "keydown", this.onKeydown);
  }

  onInputChange = ({ currentTarget: { value } }) => {
    setTimeout(this.props.onInputChange(value), 300);
  };

  onKeydown = (e) => {
    // if the user hits Escape while typing in the filter input,
    // clear the filter and un-focus the input
    if (e.keyCode == 27) {
      this.inputRef.value = '';
      this.onInputChange(e);
      setTimeout(() => this.inputRef.blur(), 1);
    }
  }

  componentWillUnmount() {
    if (this.onChangeFn) {
      this.onChangeFn();
      this.onKeydownFn();
    }
  }
}
