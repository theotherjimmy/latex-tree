# latex-tree
Dumps the tree structure of a latex document of as if by the `tree` command.
for example:
```
$ latex-tree paper.tex
paper.tex
├──format.tex
├──stdarch.tex
├──abstract.tex
├──abstract.tex
├──intro.tex
├──background.tex
├──design.tex
│  ├──figs/overview.tex
│  ├──invariant.tex
│  ├──hw-support.tex
│  ├──adversary-design.tex
│  └──api.tex
│     ├──figs/api.tex
│     └──mod-linux.tex
├──evaluation.tex
│  ├──methodology.tex
│  ├──core-channels.tex
│  ├──uncore-channels.tex
│  └──flush+reload.tex
├──discussion.tex
├──related-work.tex
├──conclusion.tex
└──notes.tex
```

## Limitations
latex-tree currently only parses `\input{...}` latex statments, and includes commented out statmests

# Installation
You should be able to install it by just running `cabal install` in the cloned/unziped repo.
