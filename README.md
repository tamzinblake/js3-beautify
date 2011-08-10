## js3-beautify ##

A Javascript pretty-printer for emacs.

Initially supports only [npm style](https://github.com/isaacs/npm/blob/master/doc/coding-style.md).

## Credits ##

Created by [Thom Blake](https://github.com/thomblake).

Based on code from [js3-mode](https://github.com/thomblake/js3-mode) so credits there are relevant.

For more credits, see https://github.com/thomblake/js3-beautify/wiki/Credits

## Installation ##

js3-beautify.el should be placed in your emacs include path. You'll need to byte-compile js3-beautify before using it - in emacs, `M-x byte-compile-file RET <path-to-js3-beautify.el> RET`.  Or on the command line: `emacs --batch -f batch-byte-compile js3-beautify.el` If you want, js3-mode can be configured using `M-x customize-group RET js3-beautify RET`.

For more details, see https://github.com/thomblake/js3-beautify/wiki/Installation

## Notes ##

Right now, all commits are the 'current development build' - so far, nothing that feels like a sufficiently stable release exists.  Several features persist from the software this is based on that may or may not be removed in the near future.

If your JS is in error, the indentation might look wrong.  I tend to regard this as a feature.

I use the default settings.

I expect that there are still some bugs; if you see any, **please report them**. Feel free to **file issue reports on github** for things like "it indented like [code block] but I want it to be [code block]".

Remember - if you start a line with `(`, `[`, `+`, or `-`, strongly consider preceding it with a semicolon (`;`).

## License ##

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see http://www.gnu.org/licenses/.
