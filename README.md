# html2reflex

Convert raw HTML to code in [Reflex-DOM](https://hackage.haskell.org/package/reflex-dom) DSL.

## Usage

Options:
- `-i,--input filename` indicates the file with HTML input.
    Better use it for larger HTML stored in a file. If absent, runs in
    interactive mode (useful for smaller pieces of HTML).
- `-o,--output filename` indicates the file for saving the result.
    If ommitted, the output will be printed to stdout. This option is ignored
    if there's no input file.
- `-n,--indent` is the number of spaces used for indentation (default: 2).
- `-h,--help` shows help text.


Simply calling `stack run` without any options will open a simple CLI where
you can insert (possibly multi-line) raw HTML extract, press Ctrl-D, and it
will be converted into Haskell code and printed. You can convert another piece
of HTML then. Press Ctrl-C to exit.
