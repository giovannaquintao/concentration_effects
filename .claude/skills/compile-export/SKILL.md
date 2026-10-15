---
name: compile-export
description: Compile misc/main.tex and save misc/main.pdf to the thesis folder in iCloud (C:\Users\giova\iCloudDrive\PhD\Thesis). Use whenever the user writes "COMPILE AND EXPORT" (any case), or asks to compile and export the PDF or send it to the thesis folder.
---

# Compile and export

Run the script in this folder from any working directory:

```bash
bash "C:/projects/concentration_effects/.claude/skills/compile-export/compile_export.sh"
```

It compiles `misc/main.tex` with `latexmk`, checks `misc/main.log` and copies
`misc/main.pdf` to `C:\Users\giova\iCloudDrive\PhD\Thesis\main.pdf`, replacing
the previous copy.

Then report to the user, in a few lines:

- number of pages and the path of the exported copy;
- any LaTeX errors (with the line numbers the script prints) and any undefined
  references or citations.

Exit codes and what to do:

- `2`: `main.pdf` is locked by a PDF viewer. Ask the user to close it; don't
  work around it.
- `3`: LaTeX produced no PDF. Report the errors; nothing was exported.
- `4`: the PDF was built but the log has LaTeX errors, so it was **not**
  exported. Report the errors and ask whether to export anyway; if the user
  says yes, run the script again with `--force`.

Don't edit `misc/main.tex` to fix errors as part of this skill: changing the
user's text needs the words "CHANGE TEXT" (see CLAUDE.md). Just report.
