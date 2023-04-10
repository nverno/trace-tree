# trace-tree - Major mode to visualize elisp trace output as tree widget

## Description

 Major mode to view elisp trace output using interactive tree widgets
 (collapse/expand children).

 Overrides `trace-make-advice` from trace.el.

### Installation

 When this file is loaded, it will simply override (using advice)
 `trace-make-advice` so the output from the `trace-*` functions in trace.el will
 be handled by this mode, instead of as plaintext.

 Place file on `load-path` and `require` somewhere before
 calling tracing functions.

Code:


---
Converted from `trace-tree.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
