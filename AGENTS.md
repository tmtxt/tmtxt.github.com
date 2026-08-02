# Agent Instructions

Rules to follow when editing content in this repository.

## Markdown tables

- When creating a markdown table, always add the CSS class `{: .table }` on the line
  immediately after the table. Example:

  ```markdown
  | Column A | Column B |
  |----------|----------|
  | value 1  | value 2  |
  {: .table }
  ```

## Mermaid charts

- When creating a mermaid chart, use a `<div class="mermaid">` wrapper instead of a normal
  markdown code block. Example:

  ```html
  <div class="mermaid">
  flowchart LR
      A --> B --> C
  </div>
  ```

- If a file contains any mermaid chart, add `mermaid: true` to the file's front matter header.
  Example:

  ```markdown
  ---
  layout: post
  title: "My Post"
  mermaid: true
  ---
  ```
