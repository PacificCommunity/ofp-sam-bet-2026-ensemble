# Job 21641 lineage audit

This audit compares the archived fitted directories from Kflow Job 19835
(`K020-tau-not-estimated-sel20c-f10-ndpen-weak-seed23-base`) and Job 21641
(`S0.90-F2-tau2-fixed`). It also checks that the ensemble base is identical to
the Job 21641 inputs before the declared ensemble axes are applied.

| Component | Job 19835 versus Job 21641 | Classification |
|---|---|---|
| `bet.ini` and `bet.tag` | Byte-identical | Unchanged data and tag structure |
| Age-length, regional scaling, MFCL configuration and maps | Byte-identical | Unchanged |
| Catch, effort and length-frequency observations | Token-identical for all 7,449 records | Unchanged |
| Weight-frequency structure | Unused WF header changed from `200/1/1/1` to `0/0/0/0`; one unused trailing WF missing-value token removed per record | Approved cleanup; no observation changed |
| Fixed M | Same diagnostic log-intercept `-2.54930339768360` and Lorenzen slope `-1` | Unchanged in the diagnostic; varied only as a declared ensemble axis |
| DM | Same Nmax 25, eight groups, fixed `fish_pars(22)=7`, and estimated `fish_pars(23)` | Unchanged |
| CPUE and all other fish flags | Identical | Unchanged |
| Selectivity | F10 remains weak non-decreasing; F33 changes to the same weak non-decreasing treatment (`flag16: 0 -> 1`, `flag56: 0 -> 10000`) | Intended Diagnostic selectivity change |
| Tag likelihood | `parest 111=4` in both; Job 21641 adds direct parameterization `parest 305=1`, fixes every `fish_pars(4)=0`, and retains fish flags 43/44 at zero | Intended `tau=2` fixed change |
| Steepness | Job 19835 fitted h=0.80; Job 21641 fitted h=0.90 | Diagnostic-grid choice; replaced by each ensemble draw |
| Initialization | Job 19835 applied deterministic seed-23 initialization at phases 1, 2 and 5; Job 21641 uses ordinary `bet.ini -makepar` without that perturbation | Intended no-seed change |

The archived final PAR files also differ at age flag 181. Neither do-it-all
script sets this flag, and MFCL comparison diagnostics identify it as an
internally set flag rather than a user model control. It is therefore not an
additional scientific setting difference.

After accounting for the explicitly requested Diagnostic selectivity, fixed-tau, steepness,
no-seed and unused-WF changes, no other data, DM, M, CPUE, tag, recruitment,
movement, growth or likelihood control difference was found.

For every ensemble run, the preparation layer may change only the declared
axes: steepness, Lorenzen M intercept, tag-mixing source, tag-reporting option
and the F29-F33 effort-creep series. The complete Diagnostic selectivity structure and
direct negative-binomial parameterization remain common to all 100 fits. Tau
is now the declared three-level fixed axis (`1.2`, `1.3`, `1.4`); runtime audits
check its selected value, steepness, M, DM and Diagnostic selectivity after
every fitted phase.
