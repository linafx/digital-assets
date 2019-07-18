---
linkTitle: "Get started"
title: "Install the DAML SDK"
weight: 10
---

# Install the DAML SDK

## 1. Install the dependencies

The SDK currently runs on Windows, MacOS or Linux.

You need to install:

1.  [Visual Studio Code](https://code.visualstudio.com/download).
2.  [JDK 8 or
    greater](http://www.oracle.com/technetwork/java/javase/downloads/index.html).

## 2. Install the SDK

### Mac and Linux

To install the SDK on Mac or Linux:

1.  Run:
    
        curl -sSL https://get.daml.com/ | sh

2.  If prompted, add `~/.daml/bin` to your PATH.

### Windows

**Note**: There's currently a known issue where the installer gets flagged as
"unrecognized" by Windows Defender, but this will be fixed soon.

We support running the SDK on Windows 10. To install the SDK on Windows,
download and run the installer from
[github.com/digital-asset/daml/releases/latest](https://github.com/digital-asset/daml/releases/latest).

## Next steps

  - Follow the [quickstart guide]({{< relref "quickstart.md" >}}).
  - Read the [introduction](introduction) page.
  - Use `daml --help` to see all the commands that the DAML assistant
    (`daml`) provides.
  - If you run into any problems, [use the support page](support/support) to get in touch with us.