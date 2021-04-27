class Component {
  constructor(props = {}) {
    this.props = props;
    this.prevProps = {};
    this.state = {};
  }

  setState(nextState, cb = () => {}) {
    if (typeof nextState === "function") {
      this.state = {
        ...this.state,
        ...nextState(this.state),
      };
    } else {
      this.state = {
        ...this.state,
        ...nextState,
      };
    }

    cb();

    if (this.render) {
      this.render();
    }
  }
}
