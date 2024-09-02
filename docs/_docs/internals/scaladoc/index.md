---
layout: doc-page
title:  Scaladoc
partof: scaladoc
overview-name: Scaladoc
num: 1

---


# Custom Template Tags Documentation


## 1. Language Picker Tag

The language_picker tag is a custom tag used to render a language selection dropdown on a page. This tag allows users to switch between different language versions of the content. The available languages are determined based on the configuration and the frontmatter of the page.

### Prequisites:
 - #### Configuration in config.yaml:
    The available languages must be defined in the `config.yaml` file under the languages section. Each language should have a corresponding code and name.
    Example:
    ```yaml
    languages:
    -   code: en
        name: English
    -   code: ja
        name: Japanese
    ```
 - #### Language Folders in `_docs`:
    For each language code specified in config.yaml, there should be a corresponding folder in the _docs directory. These folders contain the translated content for each language.
    Example:
    ```
    _docs/
    ├── en/
    ├── ja/

    ```
 - #### Frontmatter in the page:
    Each page should have a `languages` key in its frontmatter that lists the available languages for that page. This ensures that the language picker dropdown is rendered with the correct options.

    Example:
    ```yaml
    ---
    title: Getting Started
    languages: ['en', 'ja']
    ---
    ```

### Usage:

```html
<div class="language-picker-container">
    { % if page.languages % }
        { % language_picker languages=page.languages % }
    { % endif %}
</div>
```

### Considerations

- Empty Language List: If the languages list in the frontmatter is empty, the language_picker tag will not render anything.
- Default Language: If the selected language is en, the language code will be removed from the URL, treating it as the default language.
- Folder Structure: Ensure that the corresponding language folders exist in the _docs directory; otherwise, the language switch will lead to broken links.



## 1. Include Tag

### Purpose:
The `{ % include % }` tag is used to include content from other files or templates within your page. This helps in reusing common elements across multiple pages, such as headers, footers, or any other repeated content.

### Usage:
```html
{ % include 'filename' variable1='value1' variable2='value2' % }
```


### Example:
```html
{ % include 'header.html' title='Welcome to My Website' % }
```

### How it Works:
 - File Inclusion: The tag includes the specified file from a predefined folder.
 - Passing Variables: You can pass variables when including a file, which can be accessed within the included file using the syntax {{ include.variable1 }}.
 - Accessing Variables: Inside the included file, the passed variables can be accessed like regular variables, allowing dynamic content based on the context of inclusion.



## 2. Tabs  Block

### Purpose:
The { % tabs % } block is used to group content into tabs, allowing users to switch between different content sections without reloading the page. This is useful for organizing content that has multiple views, like installation instructions for different platforms.

### Usage:
```html
{ % tabs unique-tabs-id class='additional-class' % }
  { % tab 'Tab 1' for='unique-tabs-id' % }
    <!-- Content for Tab 1 -->
  {  % endtab % }

  { % tab 'Tab 2' for='unique-tabs-id' % }
    <!-- Content for Tab 2 -->
  { % endtab % }

{ % endtabs % }
```

### Example:

```html
{ % tabs install-instructions % }
  { % tab 'Windows' for='install-instructions' % }

  { % endtab % }

  { % tab 'macOS' for='install-instructions' % }

  { % endtab % }

{ % endtabs % }
```

How it Works:
- Tabs Block: The { % tabs % } tag creates a container for multiple tabs.
- Tab Content: Each { % tab %  } tag defines the content of a single tab. The for attribute must match the id specified in the { % tabs % } block to correctly link the tab content.
- Default Tab: You can specify a default tab by using the defaultTab keyword in the { % tab % } tag.
- Styling: You can apply additional CSS classes to the tabs by specifying the class attribute


## 2. AltDetails  Block

### Purpose:
The { % altDetails % } tag is used to create switchable sections, often referred to as tabs. This is useful for including optional or advanced information that doesn’t need to be visible by default.

### Usage:

```html
{ % altDetails 'unique-id' 'Title of the Section' % }
  <!-- Content inside the dropdown -->
{ % endaltDetails %}

```

### How it Works:

- Collapsible Section: The { % altDetails % } tag creates a section that can be expanded or collapsed by the user.
- Title and ID: The first argument is a unique identifier for the section, and the second argument is the title displayed when the section is collapsed.
- Content: Any content placed within the { % altDetails % } and { % endaltDetails % } tags is hidden by default and can be revealed by clicking on the title.


# `_data` Folder

The _data folder in your project is used to store YAML, JSON, or CSV files that contain structured data. These files can then be accessed throughout your site using the site.data variable. This is particularly useful for storing site-wide information, such as navigation menus, reusable content blocks, or any other structured data that needs to be referenced across multiple pages.

### Structure and Access
 - Folder Location: The _data folder should be located in the root directory of your project.

 - File Formats: The _data folder supports .yml, .yaml, .json, and .csv file formats. Each file represents a collection of data that can be accessed using the site.data variable.

 - Accessing Data:
    - The data within a YAML file can be accessed using `site.data.<file_name>.<key>.`
    - Replace `<file_name>` with the name of the file (without the extension), and `<key>` with the specific key within the file.

### Example

Suppose you have a YAML file `_data/navigation.yml` with the following content:

```yaml
main_menu:
  - name: Home
    url: /
  - name: About
    url: /about/
  - name: Contact
    url: /contact/
```

You can access the data in this file using the following syntax:

```html
<ul>
  { % for item in site.data.navigation.main_menu % }
    <li><a href="{ { item.url } }">{ { item.name } }</a></li>
  { % endfor % }
</ul>
```

### Considerations
 - File Naming: Make sure the file names are unique to avoid conflicts when accessing data.
 - Data Structure: Keep the data structure consistent across different files to avoid confusion when accessing data in your templates.

<aside class="info">
<div class='icon'></div>
<div class='content'>
It is preferred to avoid using hyphens in naming conventions. When using LIQP tags, hyphens can be misinterpreted as minus signs, leading to unexpected errors.
<br/>
{ { site.data.example['key-with-hyphen'] } }

</div>
</aside>





# Configuration File

The `_config.yaml` file is a central configuration file located in the root directory of your project. It is used to define site-wide settings and variables that can be accessed throughout your site using the `site` variable.

### Structure and Access

- File Location: The _config.yaml file should be placed in the root directory of your project.

- YAML Structure: The file uses YAML syntax to define configuration options. You can define any key-value pairs in this file, and they will be accessible via the site.config.`<key>` variable in your templates.

- Accessing Configuration:
    - Any configuration setting defined in _config.yaml can be accessed using site.`<key>`.
    - For example, if you define a key title: My Website, you can access it using site.title.

### Example
If your _config.yaml includes:
```yaml
site_name: My Awesome Site
description: A description of my awesome site.
```
You can access these values in your templates:
```html
{ { site.config.site_name } }  <!-- Outputs: My Awesome Site -->
```



<aside class="info">
<div class='icon'></div>
<div class='content'>
It is preferred to avoid using hyphens in naming conventions. When using LIQP tags, hyphens can be misinterpreted as minus signs, leading to unexpected errors.
<br/>
{ { site.data.example['key-with-hyphen'] } }
</div>
</aside>
