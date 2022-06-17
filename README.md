# Cardinal

This is a solver for [Cardinal Chains](https://danijmn.itch.io/cardinalchains):

> A minimalist puzzle game centering on the concept of non-decreasing sequences.
>
> Each puzzle begins with a monochrome grid of numbers, plus a few colored cells
> marked with an X. Starting at these cells, you must link up numbers in
> non-decreasing order until the whole grid is filled with color.

## Problem specification

Partition nodes into [Hamiltonian paths](https://en.wikipedia.org/wiki/Hamiltonian_path).
This is almost a [Hamiltonian decomposition](https://en.wikipedia.org/wiki/Hamiltonian_decomposition),
except we don't need to cover all edges.

## Analysis

- Colors are meaningless.
- There is at least one path for each sink: $|X|\geq|S|$;
  there might be more because paths can't cross each other.
- A path can't disconnect nodes.
- There are impossible puzzles, e.g. `X21`.
- [Dirac's theorem](https://en.wikipedia.org/wiki/Hamiltonian_path#Bondy%E2%80%93Chv%C3%A1tal_theorem)
  (and [its directed extension](https://cstheory.stackexchange.com/a/22239))
  don't always apply, e.g. `X1`.
- Ore's theorem doesn't apply, either.
