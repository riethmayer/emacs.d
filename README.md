# My ~/.emacs.d

- [straight](https://github.com/radian-software/straight.el) package manager
- [github copilot](https://github.com/zerolfx/copilot.el)

## OSX configuration

Make sure to unbind `C-SPC` from System Preferences > Keyboard > Shortcuts > Input Sources.

## iTerm2 configuration

### General config of key strokes

In Profiles > Keys > General:

Left Option key = ESC+

### Ansi Characters

To make `C-=` and `C--` work in iTerm2, I needed to configure how their escape characters are sent to the terminal so Emacs actually reacts to these keybindings.

https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#list-of-keyboard-strings was mega helpful, thanks so much [@fnky](https://github.com/fnky).

Changes need to be made in Profiles > Keys > Key Mappings.

#### Make `C--` work with iTerm and Emacs

- Keyboard shortcut: `^-`
- Action: Send Escape Sequence
- Esc+ [45;5u

#### Make `C-=` work with iTerm and Emacs

- Keyboard shortcut: `^=`
- Action: Send Escape Sequence
- Esc+ [61;5u

