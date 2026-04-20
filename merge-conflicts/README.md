# merge-conflicts

`merge-conflicts` is a pure-Steel example plugin for the Helix Steel branch. It provides a current-buffer UI for navigating and resolving Git merge conflicts without leaving the editor.
[See recording here](https://asciinema.org/a/WUlho6eeIP2bR1HG)!

## What it does

- parses standard Git conflict markers in the current buffer
- opens a floating resolver window with a preview of the current conflict
- jumps between conflicts
- resolves the current conflict with:
  - ours
  - theirs
  - base (when present)
  - both
  - strip markers

This first version is intentionally buffer-local. Helix already exposes conflicted files in `:changed-file-picker`, but `helix-vcs` is not exposed to Steel yet, so repo-wide dashboards are left as follow-up work.

## Loading the plugin

If this repo is on Steel's module path, add the following to `init.scm`:

```scheme
(require "helix-plugins/merge-conflicts/merge-conflicts.scm")
```

If you are loading it directly from a checkout, use the absolute path instead ($HOME won't work):

```scheme
(require "/absolute/path/to/helix-plugins/merge-conflicts/merge-conflicts.scm")
```

## Commands

- `:merge-conflicts` - open or refresh the resolver window for the current buffer
- `:merge-conflicts-refresh` - rescan the current buffer and reopen the resolver
- `:merge-conflicts-next` - jump to the next conflict
- `:merge-conflicts-prev` - jump to the previous conflict
- `:merge-conflicts-keep-ours` - keep the current branch's side
- `:merge-conflicts-keep-theirs` - keep the incoming branch's side
- `:merge-conflicts-keep-base` - keep the base section when present
- `:merge-conflicts-keep-both` - keep ours followed by theirs
- `:merge-conflicts-strip-markers` - remove the marker lines and keep all section bodies in order (`ours`, then `base` when present, then `theirs`)

The five `:merge-conflicts-keep-*` and `:merge-conflicts-strip-markers` commands operate on every conflict that overlaps the current selection. Press `%` to select the whole buffer (or extend a selection across several conflicts) and the command will apply the chosen action to all of them in one go. With no overlapping selection the action falls back to the conflict that the resolver window is currently focused on, so the modal keybindings keep their original single-conflict behavior. For `:merge-conflicts-keep-base`, conflicts without a base section are skipped and reported in the status message.


## Window keybindings

- `j` / `k` or arrow keys - next / previous conflict
- `Tab` / `Shift-Tab` - preview and cycle through ours / theirs / base / both in the background editor
- `Enter` - apply the currently selected preview action
- `o` - keep ours
- `t` - keep theirs
- `b` - keep base
- `a` - keep both
- `s` - strip markers and keep all section bodies
- `u` - undo the last accepted merge-conflict resolution inside the resolver
- `U` - redo the last resolver-local undo
- `r` - rescan the buffer
- `q` / `Esc` - close the window and restore the original conflict text if you were previewing

## Notes

- The plugin installs a couple of editor hooks when it is required. They are guarded so they only install once per runtime.
- When a conflicted file is opened, the plugin posts a status hint telling you to run `:merge-conflicts`.
- If the buffer still contains marker-looking lines but no longer parses as a valid conflict block, the plugin warns instead of applying a risky edit.
- Preview mode is temporary: cycling options rewrites the current conflict in the buffer for inspection, but closing the window or moving to another conflict restores the original conflict block until you explicitly apply an action.
