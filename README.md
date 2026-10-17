# Prototype: reconciling model domains

Minimal Scala 3 implementation of section 5 of the manuscript:

- the multi-level domain (8), the parameter and model tuples (9)–(10);
- the implicit context IC = ⟨Θ, A_txt⟩ with the numeric context check (11) and the combination of contexts at step 6;
- the consistency conditions (2), (3), (4), (5), (6); (5) is computed exactly for each implementation type,
  not sampled on a grid, and binary64 is bounded by half an ulp rather than treated as exact;
- the reconciliation checks (14)–(16) for any number of models and the sequential check (17) per level of the
  downstream input, with the model error δ and the preimage D'.

Every condition returns a three-valued verdict: `Holds`, `Violated` or `NotCheckable`. A level that was never
recorded (`Domain.domC = None`, as for reused code where only the expression and the type are known) is
`NotCheckable`, not satisfied. A sequential link is `Violated` if any level is violated, `NotCheckable` if any level
cannot be checked, `Conditional` if (17) holds but no estimate of δ was given (δ = 0 is then assumed), and
`Compatible` otherwise.

The tests recompute every number of the worked example in section 5.4 (Table 3), the accumulated-error check (6)
for the Patriot clock, the Ariane 5 link and the verdict rules above.

The sources live in `src/`: `Domain.scala` holds the levels, the tuples, the contexts and the checks of
`Procedure`, `Example.scala` the worked example of 5.4 and its report and `Example.test.scala` the tests. The
scala-cli directives sit in `project.scala` at the root, from where both commands below are run.

```bash
scala-cli run .  --main-class reconcile.report   # prints the worked example step by step
scala-cli test .                                 # 23 tests
```

Limits: this is an arithmetic demonstrator, not a toolchain component. A model has one input and one output; domains
are one-dimensional intervals; the interval extension is exact only for functions monotone on D (the example's
case), and a general [f] would need interval arithmetic or subdivision; there is no FMI or SSP import. Integer types
round to nearest and fixed point chops. Text assumptions A_txt are stored, not checked.

## Install on macOS

Homebrew carries both pieces; the Scala compiler itself is fetched by scala-cli on the first run.

```bash
brew install openjdk@21   # the version the prototype is checked on; 17 and 25 serve as well
brew install scala-cli
```

Homebrew keeps its JDKs keg-only, so the system Java wrappers find one only through a symlink:

```bash
sudo ln -sfn /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-21.jdk
```

On an Intel Mac the Homebrew prefix is `/usr/local`, so both paths in that line change accordingly. Both tools
then answer:

```bash
java -version
scala-cli version
```

Checked with scala-cli 1.16 and Scala 3.8.4; the 23 tests pass on JDK 17, 21 and 25.
