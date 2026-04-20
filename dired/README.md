# dired

`dired` is a pure-Steel, Dired-style directory browser for the Helix Steel branch. It gives you a navigable file listing, lets you descend into directories, open files, and perform a small set of common file operations without leaving the editor.

## What it does

- opens a directory listing rooted at the current file's directory, or Helix's cwd
- shows directories before files
- supports parent-directory traversal with a dedicated `..` entry
- opens files in Helix and descends into directories from the same picker-style UI
- supports:
  - refresh
  - create file
  - create directory
  - rename or move
  - delete

This is an MVP focused on core browsing plus single-entry file operations. It does **not** yet include Dired-style marks, batch actions, git decorations, or metadata columns.

## Loading the plugin

If this repo is on Steel's module path, add the following to `init.scm`:

```scheme
(require "helix-plugins/dired/dired.scm")
```

If you are loading it directly from a checkout, use the absolute path instead:

```scheme
(require "/absolute/path/to/helix-plugins/dired/dired.scm")
```

## Commands

- `:dired` - open the browser for the current Dired directory, or the current file's directory if no Dired session exists yet
- `:dired-refresh` - rescan the current directory
- `:dired-next` - move to the next entry
- `:dired-prev` - move to the previous entry
- `:dired-open` - open the selected file or descend into the selected directory
- `:dired-up-directory` - move to the parent directory
- `:dired-create-file` - prompt for a new file path
- `:dired-create-directory` - prompt for a new directory path
- `:dired-rename` - prompt for a new path for the selected entry
- `:dired-delete` - prompt for delete confirmation

## Window keybindings

- `j` / `k` or arrow keys - next / previous entry
- `Enter`, `l`, or `o` - open the selected entry
- `h` or `-` - visit the parent directory
- `g` / `G` - jump to the first / last entry
- `r` - refresh the listing
- `n` - create a new file
- `+` - create a new directory
- `R` - rename or move the selected entry
- `D` - delete the selected entry after confirmation
- `q` / `Esc` - close the window

## Notes

- Paths entered in prompts can be absolute or relative to the directory currently shown in Dired.
- Rename or move is implemented entirely in Steel. Directory moves copy recursively and then remove the old directory; regular-file moves copy bytes and then delete the original file.
- Because of that pure-Steel implementation, rename or move does not currently preserve file metadata like mtimes or permissions.
