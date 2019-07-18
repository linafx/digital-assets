---
title: "DAML Studio"
weight: 30
description: >
  DAML Studio is an integrated development environment (IDE) for DAML.
---

It is an extension on top of [Visual Studio
Code](https://code.visualstudio.com) (VS Code), a cross-platform,
open-source editor providing a [rich code editing
experience](https://code.visualstudio.com/docs/editor/editingevolved).

## Installing

To install DAML Studio, [install the SDK]({{< relref "/docs/get-started/_index.md" >}}). DAML Studio isn't currently available
in the Visual Studio Marketplace.

## Creating your first DAML file

1.  Start DAML Studio by running `daml studio` in the current project.
    
    This command starts Visual Studio Code and (if needs be) installs
    the DAML Studio extension, or upgrades it to the latest version.

2.  Make sure the DAML Studio extension is installed:
    
    1.  Click on the Extensions icon at the bottom of the VS Code
        sidebar.
    2.  Click on the DAML Studio extension that should be listed on the
        pane.
    
    ![image](daml-studio/images/daml_studio_extension_view.png)

3.  Open a new file (`⌘N`) and save it (`⌘S`) as `Test.daml`.

4.  Copy the following code into your file:

	<div class="literalinclude" data-language="daml">

	daml-studio/daml/Test.daml

	</div>

	Your screen should now look like the image below.

	> ![image](daml-studio/images/daml_studio_extension_double_correct.png)

5.  Introduce a parse error by deleting the `=` sign and then clicking
    the Ⓧ symbol on the lower-left corner. Your screen should now look
    like the image below.
    
    ![image](daml-studio/images/daml_studio_extension_double_wrong.png)

6.  Remove the parse error by restoring the `=` sign.

We recommend reviewing the [Visual Studio Code
documentation](https://code.visualstudio.com/docs/editor/codebasics) to
learn more about how to use it.

## Supported features

Visual Studio Code provides many helpful features for editing DAML files
and Digital Asset recommends reviewing [Visual Studio Code
Basics](https://code.visualstudio.com/docs/editor/codebasics) and
[Visual Studio Code Keyboard Shortcuts for OS
X](https://code.visualstudio.com/shortcuts/keyboard-shortcuts-macos.pdf).
The DAML Studio extension for Visual Studio Code provides the following
DAML-specific features:

### Symbols and problem reporting

Use the commands listed below to navigate between symbols, rename them,
and inspect any problems detected in your DAML files. Symbols are
identifiers such as template names, lambda arguments, variables, and so
on.

| Command                       | Shortcut (OS X) |
| ----------------------------- | --------------- |
| [Go to Definition]()          | `F12`           |
| [Peek Definition]()           | `⌥F12`          |
| [Rename Symbol]()             | `F2`            |
| [Go to Symbol in File]()      | `⇧⌘O`           |
| [Go to Symbol in Workspace]() | `⌘T`            |
| [Find all References]()       | `⇧F12`          |
| [Problems Panel]()            | `⇧⌘M`           |


TODO rest of file