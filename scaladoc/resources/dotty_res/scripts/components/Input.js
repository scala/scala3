class Input extends Component {
  constructor(props) {
    super(props);

    this.inputRef = findRef(".filterableInput");
    this.onChangeFn = withEvent(this.inputRef, "input", this.onInputChange);
  }

  onInputChange = ({ currentTarget: { value } }) => {
    this.props.onInputChange(value);
  };

  componentWillUnmount() {
    if (this.onChangeFn) {
      this.onChangeFn();
    }
  }
}
