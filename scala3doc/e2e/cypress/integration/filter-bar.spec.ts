describe('filter-bar', () => {
  beforeEach(openPage);

  it('properly renders filters extracted from the document', () => {
    const filterBar = new FilterBarFixture().toggle();

    const testTable: TestTable = [
      ['Visibility', ['public', 'protected']],
      ['Keywords', ['no keywords', 'abstract', 'case', 'final', 'sealed']],
      ['Extension', ['Standard member', 'from tests']],
    ];

    testFilterBarOptions(filterBar, testTable);
  });

  it('properly filters the definition list through search box', () => {
    const tabs = new TabsFixture();
    const filterBar = new FilterBarFixture().toggle();

    // type
    tabs.definition('publicType').should('be.visible');
    // protected type
    tabs.definition('protectedType').should('be.visible');

    filterBar.input.type('protectedType');

    // protected type
    tabs.definition('protectedType').should('be.visible');
    // type
    tabs.definition('publicType').should('not.be.visible');

    const testTable: TestTable = [
      ['Visibility', ['public', 'protected']],
      ['Keywords', ['no keywords']],
      ['Extension', ['Standard member']],
    ];

    testFilterBarOptions(filterBar, testTable);
  });

  it('works with select all / deselect all', () => {
    const filterBar = new FilterBarFixture().toggle();
    const group = filterBar.group(0);
    const batchSelection = filterBar.group(0).batchSelection;

    const public = () => group.filterOption('public').then(x => x.isSelected);
    const protected = () => group.filterOption('protected').then(x => x.isSelected);

    public().should('be.equal', true);
    protected().should('be.equal', true);

    batchSelection.deselectAll();

    public().should('be.equal', false);
    protected().should('be.equal', false);

    batchSelection.selectAll();

    public().should('be.equal', true);
    protected().should('be.equal', true);
  });

  describe('filter configurations', () => {
    describe('returns empty list after deselecting', () => {
      it(`'public' and 'no keywords'`, () => {
        const filterBar = new FilterBarFixture().toggle();
        filterBar.group(0).toggleFilter('public');
        filterBar.group(1).toggleFilter('no keywords');

        new TabsFixture().definitionTypes.should('not.be.visible');
      });

      it(`'Standard member'`, () => {
        new FilterBarFixture().toggle().group(2).toggleFilter('Standard member');

        new TabsFixture().definitionTypes.should('not.be.visible');
      });

      it('all visibility options', () => {
        new FilterBarFixture().toggle().group(0).toggleFilter('public', 'protected');

        new TabsFixture().definitionTypes.should('not.be.visible');
      });

      it('all keywords options', () => {
        new FilterBarFixture()
          .toggle()
          .group(1)
          .toggleFilter('no keywords', 'abstract', 'case', 'final', 'sealed');

        new TabsFixture().definitionTypes.should('not.be.visible');
      });

      it('all extension options', () => {
        new FilterBarFixture().toggle().group(2).toggleFilter('Standard member', 'from tests');

        new TabsFixture().definitionTypes.should('not.be.visible');
      });
    });

    describe('returns filtered list after deselecting', () => {
      it(`'protected'`, () => {
        const tabs = new TabsFixture();

        tabs.definition('protected').should('be.visible');
        new FilterBarFixture().toggle().group(0).toggleFilter('protected');
        tabs.definition('protected').should('not.be.visible');
      });

      it(`'no keywords', 'case', 'final' and 'sealed'`, () => {
        const tabs = new TabsFixture();

        // protected object
        tabs.definition('ProtectedObject').should('be.visible');

        // sealed case class
        tabs.definition('D').should('be.visible');

        // final case class
        tabs.definition('E').should('be.visible');

        new FilterBarFixture()
          .toggle()
          .group(1)
          .toggleFilter('no keywords', 'case', 'final', 'sealed');

        // protected object
        tabs.definition('ProtectedObject').should('not.be.visible');

        // sealed abstract class
        tabs.definition('B').should('be.visible');

        // abstract case class
        tabs.definition('C').should('be.visible');

        // sealed case class
        tabs.definition('D').should('not.be.visible');

        // final case class
        tabs.definition('E').should('not.be.visible');
      });

      it(`'no keywords', 'final' and 'sealed'`, () => {
        const tabs = new TabsFixture();

        // protected object
        tabs.definition('ProtectedObject').should('be.visible');

        new FilterBarFixture().toggle().group(1).toggleFilter('no keywords', 'final', 'sealed');

        // protected object
        tabs.definition('ProtectedObject').should('not.be.visible');

        // sealed abstract class
        tabs.definition('B').should('be.visible');

        // abstract case class
        tabs.definition('C').should('be.visible');

        // sealed case class
        tabs.definition('D').should('be.visible');

        // final case class
        tabs.definition('E').should('be.visible');
      });
    });
  });
});

class FilterBarFixture {
  private get toggleButton() {
    return cy.findByTestId('filterToggleButton');
  }

  group(at: number) {
    return new FilterBarGroupFixture(at);
  }

  get input() {
    return new FilterInputFixture();
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

  private get filterButtons() {
    return this.group
      .findByTestId('filterGroupList')
      .findAllByTestId('filterGroupButton')
      .filter(':visible');
  }

  get title() {
    return this.group.findByTestId('filterGroupTitle');
  }

  get batchSelection() {
    return new BatchSelectionFixture(() => this.group);
  }

  get filterOptionsValues() {
    return this.filterOptions.then(options => {
      const acc: string[] = [];
      options.forEach(o => o.name.then(v => acc.push(v)));
      return cy.wrap(acc);
    });
  }

  filterOption(name: string) {
    return this.filterButtons
      .contains(name)
      .then($el => new FilterOptionFixture(() => cy.wrap($el)));
  }

  get filterOptions() {
    return (
      this.filterButtons
        //   .filter(':visible')
        .then($buttons =>
          cy.wrap($buttons.toArray().map(el => new FilterOptionFixture(() => cy.wrap(el)))),
        )
    );
  }

  toggleFilter(...names: string[]) {
    names.forEach(name => this.filterButtons.contains(name).click());
    return this;
  }
}

class FilterOptionFixture {
  constructor(private readonly root: () => Cypress.Chainable<JQuery<HTMLElement>>) {}

  get name() {
    return this.root().then($el => $el.text());
  }

  get isSelected() {
    return this.root().then($el => cy.wrap($el.data('selected')));
  }
}

class TabsFixture {
  get definitionTypes() {
    return cy.findAllByTestId('definitionList');
  }

  definition(name: string) {
    return this.definitionTypes.contains(name);
  }
}

class FilterInputFixture {
  private get input() {
    return cy.findByTestId('filterBarInput');
  }

  type(phrase: string) {
    this.input.type(phrase);
  }
}

class BatchSelectionFixture {
  constructor(private readonly root: () => Cypress.Chainable<JQuery<HTMLElement>>) {}

  private get container() {
    return this.root().findByTestId('filterGroupBatchToggle');
  }

  selectAll() {
    this.container.findByText('Select All').click();
  }

  deselectAll() {
    this.container.findByText('Deselect All').click();
  }
}

function openPage() {
  cy.visit('http://localhost:8080/testcases/api/tests/-filter-test/index.html');
}

type TestTable = [string, string[]][];

function testFilterBarOptions(filterBar: FilterBarFixture, testTable: TestTable) {
  testTable.forEach(([title, filterOptions], index) => {
    const group = filterBar.group(index);

    group.title.should('have.text', title);
    group.filterOptionsValues.should('deep.equal', filterOptions);
  });
}
