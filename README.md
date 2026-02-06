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

`M-x linear` brings up a list of tickets.

| Key   | Action                          |
|-------|---------------------------------|
| `RET` | Show issue details              |
| `o`   | Open issue in browser           |
| `C-w` | Copy issue URL to kill ring     |
| `s`   | Switch view                     |
| `g`   | Refresh                         |
| `n/p` | Next/previous line              |

### Views

Customize `linear-views` to define named query views. Each view has a `:filter`
plist supporting `:assignee`, `:states`, `:team`, and `:project`:

```elisp
(setq linear-views
      '((:name "My Issues"
         :filter (:assignee me))
        (:name "My Todo"
         :filter (:assignee me :states ("Todo")))
        (:name "Team CHO"
         :filter (:team "CHO"))))
```

Press `s` in the linear buffer to switch between views.

## Development

Requires the rust toolchain installed for the mock linear server.

```
make test              # all tests
make test-unit         # unit tests only
make test-integration  # tests that use the mock server
```

[linear]: https://linear.app
