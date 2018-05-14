# Generalized Algebraic Data Type (GADT) Inference

A different version of [this approach](https://www.microsoft.com/en-us/research/publication/complete-and-decidable-type-inference-for-gadts/), using lcg (least common generalization) to infer types from case expressions, and supporting type signature through reducing the extra information generated and doing exactly what OutsideIn does.

An entry has multiple data declarations followed by one expression. See `examples`.

Run `generate "file"` to get generated constraints.

Run `solve "file"` to get the type inferred.
