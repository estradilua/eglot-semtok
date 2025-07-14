# Eglot-semtok

This package is more-or-less of a direct port of `lsp-mode`'s Semantic Tokens support for Eglot. After a lot of thought and experimentation, I became convinced that their implementation already does most things in the optimal way, and therefore there was no point in rewriting the wheel from scratch.

Even if the overall logic and algorithms are taken from `lsp-mode`'s implementation, the code in this package has been made idiomatic and compatible with the Eglot ecosystem by an extensive use of `eieio` facilities, simpler user-facing options and a removal of `dash`. A small logic change is that, after modifying a document, the Eglot client will make a `full/delta` request instead of a `range` request if `full` information is available the server supports that. This should be more optimal, considering that `delta` requests are usually cheap and that it prevents a subsequent `full` request that would be triggered on idle, saving one request.

## Usage

To use this package with a server that supports semantic tokens, simply prepend the symbol `eglot-semtok-server` before the server in your `eglot-server-programs` entry, like so:

```elisp
(add-to-list 'eglot-server-programs '(my-mode eglot-semtok-server "my-lsp" "command"))
```

This will make so that during initialization, Eglot will instantiate the server using the `eglot-semtok-server` class.
