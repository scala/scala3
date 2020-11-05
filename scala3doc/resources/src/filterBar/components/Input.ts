import { Component } from '../common/Component';
import { findRef } from '../common/util';

type Props = {
  onInputChange: (value: string | null) => void;
};

export class Input extends Component<Props> {
  constructor(props: Props) {
    super(props);

    findRef('.filterableInput').addEventListener('input', e => this.onInputChange(e));
  }

  onInputChange = ({ currentTarget }: Event) => {
    this.props.onInputChange((currentTarget as HTMLInputElement).value);
  };
}
