# emacs
my emacs setup
name this repo as .emacs.d, and import it into your .emacs file like this:


```elisp
(load-file '~/.emacs.d/stephen.el')
```

You might need to install `use-package` and `exec-path-from-shell` beforehand.

# What I use Emacs for
* js / ts
* rust
* go

# emacs dependencies
* use-package : need this to get all the other dependencies!  just `package-install` it.

# outside of emacs dependencies
Highkey assumes you're using MacOSX. Sorry.

Other dependencies:
* fzf
* TODO: pbcopy (should probably make this os-agnostic soon)

