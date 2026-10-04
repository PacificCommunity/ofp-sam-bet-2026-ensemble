# Runtime

Reruns use the pinned Linux amd64 TunaFlow v2.5 image and verify MFCL SHA-256 `f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0`. Runtime updates are disabled.

For offline use, reconstruct the saved image with Python 3.10+:

```sh
python3 rr-test/runtime/tools.py download --archive runtime-80577e91a76c1776a4d2509350c9251d7e3eceb8cbb550a33b652bf7df6d52f4
python3 rr-test/runtime/tools.py save-archive --archive runtime-80577e91a76c1776a4d2509350c9251d7e3eceb8cbb550a33b652bf7df6d52f4 --output runtime-original.tar.gz
docker load --input runtime-original.tar.gz
./rr-test/rerun rrtest-005-rr1 --outputs-only --saved-runtime
```

The download checks four parts and the complete image hash before writing a fresh file. The 1.83 GB image is shared by all reruns. `--saved-runtime` selects exact loaded image ID `sha256:898925450ab7ec3f99cb9d8427cc37d24097cc2fe9f0468d74f70aaf35daec3a` and refuses pulls. CI also evaluates central outputs directly on Ubuntu using the 34.55 MB static executable.
