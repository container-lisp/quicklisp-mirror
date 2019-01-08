# quicklisp-mirror

This lisp-powered container hosts a complete mirror of the
[Quicklisp](https://www.quicklisp.org/beta) archive, suitable for
deployment within kubernetes.

This container is available on dockerhub as
[`containerlinux/quicklisp-mirror`](https://cloud.docker.com/u/containerlisp/repository/docker/containerlisp/quicklisp-mirror/general).
Note: it is very large (> 4GB).

Container images built in OpenShift with
[`s2i-lisp`](https://github.com/container-lisp/s2i-lisp) will
automatically pick up this mirror if deployed within the same project.
This is done by [patching the quicklisp client
code](https://github.com/container-lisp/s2i-lisp/commit/7367dcda4d70bf24eb995b1cdc211c2153de3fd4)
in the s2i builder image.  Better suggestions [welcome](https://github.com/container-lisp/s2i-lisp/issues/new)!

Thanks, as always, to Zach Beane for
[Quicklisp](https://www.quicklisp.org/beta).


