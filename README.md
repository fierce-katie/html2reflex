# html2reflex

Convert raw HTML to code in [Reflex-DOM](https://hackage.haskell.org/package/reflex-dom) DSL.

## Usage

### Interactive

More useful for smaller pieces of HTML.

`stack run` will open a simple CLI where you can insert (possibly multi-line)
raw HTML extract, press Ctrl-D, and it will be converted into Haskell code and
printed. You can convert another piece of HTML then. Press Ctrl-C to exit.

### Input file

Better use it for larger HTML stored in a file.

Options:
- `-i,--input filename` indicates the file with HTML input
- `-o,--output filename` indicates the file for saving the result.
    If ommitted, the output will be printed to stdout. This option is ignored
    if there's no input file.
- `-h,--help` shows help text.

## TODOs and known bugs

- [ ] Make number of spaces used for alignment configurable
