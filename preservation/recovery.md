# Recover native bytes from RDS

Most readers can restore the saved native files directly using the
[archive guide](README.md). To check a recovery independently, use the original
RDS and the corresponding record in [recovery.json](recovery.json) or the
[Stepwise source proof](evidence/stepwise-native-model-proof.json).

The helper needs base R and Python 3. For Job 22021, obtain the original
`retro_info.rds` member and use its recorded hashes:

```sh
Rscript --vanilla preservation/recover-native.R \
  --rds retro_info.rds \
  --rds-sha256 9026a59277b280d88e4da3f190c157b277b56b5e409923788eb908b96f976b4c \
  --selector artifacts/files/par/bytes --compression gzip \
  --native-sha256 40b6445ebeec09b2eb9f5545b370ab1b7eb550354ced4ff9fbe4f8fb598f37d2 \
  --output final.par
```

The selector follows exact list names separated by `/`. Escape `~` as `~0`
and `/` as `~1`; `i:1` selects list element 1, while `n:i:1` means the literal
name `i:1`. Use `none` for uncompressed raw vectors. R's `gzip` setting here
also handles its stored zlib byte streams.

Only raw bytes are accepted. Source and output hashes must match; missing or
ambiguous selectors, numeric values, links and existing outputs are rejected.
The output parent must already exist. Use the RDS hash from the same source
record: a later Git copy can have a different serialization hash even when
its embedded native PAR is identical.
