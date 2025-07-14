# Eglot-semtok

This package is a port of `lsp-mode`'s Semantic Tokens support for Eglot. After thought and experimentation, I became convinced that their implementation already does most things in the optimal way, and therefore there was no point in rewriting the wheel from scratch.

Even if the overall logic and algorithms are taken from `lsp-mode`'s implementation, the code in this package was made more idiomatic and compatible with the Eglot ecosystem by the use of `eieio`, simpler user-facing options and a removal of `dash`.

## Usage

To use this package with a server that supports semantic tokens, prepend the symbol `eglot-semtok-server` before the server in your `eglot-server-programs` entry like so:

```elisp
(add-to-list 'eglot-server-programs '(my-mode eglot-semtok-server "my-lsp" "command"))
```

This will make so that during initialization, Eglot will instantiate the server using the `eglot-semtok-server` class.
