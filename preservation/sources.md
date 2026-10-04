# Saved source versions

[Source inventory](sources.json) records each repository, available commits and
unresolved versions. Git bundles retain the available source history independently
of the original repository.

For Diagnostic Job 21641, from this repository root:

```sh
python3 preservation/tools.py download --bundle source-diagnostic
git clone preservation/downloads/diagnostic.bundle diagnostic-source
git -C diagnostic-source checkout --detach 3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6
```

Use the commit recorded for your selected job. Where a source repository is
unavailable, the inventory identifies exact commits recovered from alternate
bundles; unresolved commits remain listed.

The `published-pages-20261004` download retains the actual published HTML and
linked local assets. [Published file hashes](published-files.json) identify that
snapshot. Some interactive pages still use external services or libraries.
