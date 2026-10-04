# Saved runtime

The archive retains the pinned `tuna-flow` Linux/amd64 image as an exact
compressed Docker-save file. Download its four parts and reconstruct it from the
repository root:

```sh
python3 preservation/tools.py download --archive runtime-80577e91a76c1776a4d2509350c9251d7e3eceb8cbb550a33b652bf7df6d52f4
python3 preservation/tools.py save-archive --archive runtime-80577e91a76c1776a4d2509350c9251d7e3eceb8cbb550a33b652bf7df6d52f4 --output runtime-original.tar.gz
docker load --input runtime-original.tar.gz
docker image inspect sha256:898925450ab7ec3f99cb9d8427cc37d24097cc2fe9f0468d74f70aaf35daec3a
```

Reconstruction checks the parts and complete compressed-file hash before creating
a fresh output file. [Docker load](https://docs.docker.com/reference/cli/docker/image/load/)
accepts compressed archives. Use the recorded image ID with `--pull=never`;
registry tags and digests may not be retained by loading an image saved by ID.

[Runtime evidence](evidence/runtime.json) and [static byte identities](evidence/runtime-static-byte-identity.json)
keep the image, its engine and the repository's bundled executable distinct.
The external engine hash was not recorded for some original fits. Historical
private package updates are a further limit. Static checks preserve the image;
they do not validate a new fit. Continue with the [refit example](refit.md).
