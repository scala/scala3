describe('filter-bar', () => {
  beforeEach(() => cy.visit('http://localhost:8080/testcases/api/tests/-filter-test/index.html'));

  it('properly renders filters extracted from the document', () => {
    const filterBar = new FilterBarFixture().toggle();

    const testTable = [
      ['Visibility', ['public', 'protected']],
      ['Keywords', ['no keywords', 'abstract', 'case', 'final', 'sealed']],
      ['Extension', ['Standard member', 'from tests']],
    ];

    testTable.forEach(([title, filterOptions], index) => {
      const group = filterBar.group(index);

      group.title.should('have.text', title);
      group.filterOptions.should('deep.equal', filterOptions);
    });
  });

  it('filters by && across groups', () => {});
});

class FilterBarFixture {
  private get toggleButton() {
    return cy.findByTestId('filterToggleButton');
  }

  group(at: number) {
    return new FilterBarGroupFixture(at);
  }

  toggle() {
    this.toggleButton.click();

    return this;
  }
}

class FilterBarGroupFixture {
  constructor(private readonly index: number) {}

  private get group() {
    return cy.findAllByTestId('filterGroup').eq(this.index);
  }

  private get filterList() {
    return this.group.findByTestId('filterGroupList');
  }

  get title() {
    return this.group.findByTestId('filterGroupTitle');
  }

  get filterOptions() {
    return this.filterList
      .findAllByTestId('filterGroupButton')
      .then($buttons => cy.wrap($buttons.toArray().map(i => i.innerText)));
  }
}
