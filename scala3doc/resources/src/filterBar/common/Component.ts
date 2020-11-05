export abstract class Component<P = {}, S = {}> {
  protected state: S = {} as S;

  constructor(protected props = {} as P) {}

  render(props: P) {}

  setState(nextState: (s: S) => Partial<S>) {
    this.state = {
      ...this.state,
      ...nextState(this.state),
    };

    this.render(this.props);
  }
}
