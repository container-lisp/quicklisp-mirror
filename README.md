# quicklisp-mirror

This lisp-powered container hosts a complete mirror of the
[quicklisp](https://www.quicklisp.org/beta) archive, suitable for
deployment within kubernetes.

This container is available on dockerhub as
[`containerlinux/quicklisp-mirror`](https://cloud.docker.com/u/containerlisp/repository/docker/containerlisp/quicklisp-mirror/general).

Container images built in OpenShift with
[`s2i-lisp`](https://github.com/container-lisp/s2i-lisp) will
automatically pick up this mirror if deployed within the same project.

Thanks, as always, to Zach Beane for
[quicklisp](https://www.quicklisp.org/beta).


