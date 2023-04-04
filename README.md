linear.el
=========

A little emacs UI for [Linear][linear], very WIP.

## Installing

Clone this repo to somewhere on your emacs load path:

```
cd ~/.emacs.d/site-lisp/
git clone https://github.com/mjhoy/linear-emacs.git
```

## Configuring

You'll need a Linear API key: go to your workspace settings > API.

Add this line to your `auth-sources` file (e.g., `.authinfo`):

```
machine api.linear.app password <API_KEY>
```

In your emacs init, require `linear`:

```elisp
(require 'linear)
```

## Usage

`M-x linear` brings up a list of your assigned, in-progress tickets.

- `RET` on a ticket opens in browser.
- `C-w` copies the URL to the kill ring.

[linear]: https://linear.app
