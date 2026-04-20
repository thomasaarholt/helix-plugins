# Helix Steel plugin

## Goal

Continue work on Helix plugins written in Steel, with the current focus being the example merge-conflicts plugin under `merge-conflicts/`.

## Start by reading

1. `STEEL.md`: https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md
2. `Steel Book`: https://mattwparas.github.io/steel/book/
3. `merge-conflicts/merge-conflicts.scm`
4. `merge-conflicts/core/model.scm`
5. `merge-conflicts/ui/window.scm`
6. `merge-conflicts/README.md`
7. Reference examples:
   - `examples/scooter/scooter.scm`: https://github.com/thomasschafer/scooter.hx/blob/main/scooter.scm
   - `examples/scooter/ui/window.scm`: https://github.com/thomasschafer/scooter.hx/blob/main/ui/window.scm

## Important context

- This plugin is intentionally **pure Steel**. Do not require Rust/runtime changes unless explicitly requested.
- It is a **buffer-local merge conflict resolver**, not a repo-wide dashboard.
- `helix-vcs` conflict iteration is not exposed to Steel, so workspace-wide conflict UIs would need shell/git integration or new bindings.
- Follow the `scooter` package layout and the `picker` dynamic-component/event/render style.
- Hooks and global keybindings must be installed only once per runtime.

## Current plugin behavior to preserve

- Parses Git conflict blocks (`<<<<<<<`, optional `|||||||`, `=======`, `>>>>>>>`).
- Opens a floating modal UI for the current buffer.
- Supports actions: ours, theirs, base, both, strip.
- `strip` means: remove marker lines and keep all section bodies in order (`ours`, then `base` if present, then `theirs`).
- Supports live preview cycling with `Tab` / `Shift-Tab`.
- Inside the modal:
  - `j/k` move
  - `Enter` apply selected preview
  - `o/t/b/a/s` apply actions
  - `u` local undo of the last accepted resolver action
  - `U` local redo of the last resolver-local undo
- The resolver uses its own local undo/redo stack because Helix's global undo groups edits too coarsely for this workflow.
- Modal layout was tightened to better fit narrow windows; keep it compact.

## Important implementation constraints

- Avoid relying on Helix global undo/redo for resolver-local behavior.
- Preserve preview restoration behavior on close/navigation/refresh.
- Keep the plugin resilient to older installed Steel wrapper files; prefer APIs already exposed in the running Steel environment.
- Be careful with Scheme delimiter balance; this file had prior parse issues.
- If docs disagree with source, trust the source and then update docs.

## Useful Steel/Helix APIs already used (These must be found on a helix repo that has checked out the Steel plugin branch)

- `helix/editor.scm`
- `helix/components.scm`
- `helix/misc.scm`
- `helix/static.scm`
- `helix/core/text`

## Validation guidance

- For pure Scheme syntax, use delimiter-balance checks and/or `steel ast` when possible.
- Note that standalone `steel ast` can fail later on Helix-specific builtins; distinguish parse failures from missing builtins.
- Prefer focused validation of touched Steel files and behavior-safe edits.

## When making changes

- Keep edits surgical.
- Update `examples/merge-conflicts/README.md` when behavior change.
- Do not introduce Rust changes unless the user explicitly asks for runtime support.
