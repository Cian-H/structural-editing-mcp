# Multi-stage Alpine Docker build for structural-editing-mcp
FROM alpine:latest AS builder

RUN apk add --no-cache \
    sbcl \
    zstd-libs \
    curl \
    ca-certificates \
    git

# Install Quicklisp and pre-load all project dependencies
RUN curl -fsSL https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp && \
    sbcl --non-interactive --no-userinit \
      --load /tmp/quicklisp.lisp \
      --eval '(quicklisp-quickstart:install :path #P"/root/quicklisp/")' \
      --eval '(ql:quickload (list "trivia" "alexandria" "serapeum" "yason" "cl-indentify" "bordeaux-threads" "rove") :silent t)' \
      --eval '(sb-ext:exit :code 0)' && \
    rm /tmp/quicklisp.lisp

WORKDIR /build
COPY . /build

# Build and verify standalone executable
RUN OUTPUT_BINARY="semcp" sbcl --no-userinit --disable-debugger \
      --script scripts/build.lisp && \
    chmod +x semcp && \
    ./semcp --version

# Minimal runtime image (~22 MB total)
FROM alpine:latest

RUN apk add --no-cache zstd-libs ca-certificates

COPY --from=builder /build/semcp /usr/local/bin/semcp

ENTRYPOINT ["/usr/local/bin/semcp"]
