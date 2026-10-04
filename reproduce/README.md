# Saved native inputs

This compact package preserves exact native inputs and original scripts for 80 retained ensemble models and 30 completed reporting-rate reruns. Original ensemble PARs and whole REPs remain in [final-par/](../final-par/); the package contains the 30 RR1 final PARs and exact central REP sections. [RR results](../rr-test/results.md) can be read immediately.

Check archived bytes without downloading or running a model:

```sh
python3 reproduce/restore.py --verify
```

Restore one case to a new folder, including the checksum-checked preserved executable:

```sh
python3 reproduce/restore.py rrtest-005-rr1 /tmp/bet-rr005
```

For central-output regeneration, use [the RR helper](../rr-test/README.md). Failed RR1 fits have no saved final PAR. Original external engine identity remains unknown where it was not recorded; compatibility of the selected executable is checked separately.

Restoration provides evaluation inputs. Full refits use the original repository preparation and runners, which also create nested configuration and selectivity files.
