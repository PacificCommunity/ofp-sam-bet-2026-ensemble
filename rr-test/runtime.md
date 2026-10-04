# Runtime

The default `--outputs-only` path uses Linux x86_64 and the preserved executable from [the pinned jitter repository](https://github.com/PacificCommunity/ofp-sam-bet-2026-jitter/blob/bb3f4016b2d145f42c7a76072ed2b10b49aff71f/data/diagnostic/mfcl/mfclo64). It downloads 34.55 MB, checks SHA-256 `f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0`, restores exact inputs and evaluates the saved PAR. No R packages or container image are needed.

Use a verified local copy with `--mfcl /path/to/mfclo64`. Add `--docker` for the historical Linux amd64 TunaFlow v2.5 image, including emulation on other architectures. Full refits use that image, original R preparation and the phase 10/11 criterion `-4`. Runtime updates are disabled and fitting runs without network access.

For the preserved image, the existing offline commands remain:

```sh
python3 rr-test/runtime/tools.py download --archive runtime-80577e91a76c1776a4d2509350c9251d7e3eceb8cbb550a33b652bf7df6d52f4
python3 rr-test/runtime/tools.py save-archive --archive runtime-80577e91a76c1776a4d2509350c9251d7e3eceb8cbb550a33b652bf7df6d52f4 --output runtime-original.tar.gz
docker load --input runtime-original.tar.gz
./rr-test/rerun rrtest-005-rr1 --outputs-only --saved-runtime
```

The 1.83 GB image is optional for saved-PAR evaluation. `--saved-runtime` selects exact loaded image ID `sha256:898925450ab7ec3f99cb9d8427cc37d24097cc2fe9f0468d74f70aaf35daec3a` and refuses image pulls.

The evaluation preserves the original complete-output controls and uses one function evaluation. It requires 1,997 parameters, objective agreement within `1e-6`, and central biomass/MSY agreement within `1e-10` relative to the original REP values. RR1 references retain exact selected section bytes and the whole original REP checksum separately.

The summary uses mean SB in 2021–2024 divided by mean SB_F=0 in 2014–2023; annual depletion uses the matching year. F/F_MSY is the reciprocal of the native REP's F multiplier at MSY. Positive-mixing tag rows change INI column 2 from 0 to 1; zero-mixing rows retain sentinel 1. These historical pre-fix central estimates do not establish the corrected v2.6 response or an unbiased setting.

Saved-PAR evaluation writes a central REP, evaluated PAR, log and verification receipt. Hessians, optimiser history and stochastic projections are outside this command.
