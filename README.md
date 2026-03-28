# autoresearch_wolfram_language

Wolfram Language port of [Karpathy's autoresearch](https://github.com/karpathy/autoresearch). Claude Code autonomously experiments on a linear regression problem, looping until you stop it.

<p align="center">
  <img src="autoresearch_diagram.svg" width="500" alt="autoresearch diagram">
</p>

## Quick start

```bash
claude --dangerously-skip-permissions
```

Then ask it:

```
Hi have a look at program.md and let's kick off a new experiment! let's do the setup first.
```

## Assumptions

This repo assumes you:

- already have `wolframscript` installed and available on your command line
- already know how to run `wolframscript` from a terminal
- already have a local Wolfram kernel available for `wolframscript` to use

Official Wolfram docs:

- Install WolframScript: https://reference.wolfram.com/language/workflow/InstallWolframScript.html
- `wolframscript` command-line reference: https://reference.wolfram.com/language/ref/program/wolframscript.html
