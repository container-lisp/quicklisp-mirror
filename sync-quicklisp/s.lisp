(ql:quickload :quicklisp-mirror-tool)

(in-package :quicklisp-mirror-tool.dist)

(fetch-quicklisp.txt)
(fetch-quicklisp-versions.txt)

(sb-ext:quit)


