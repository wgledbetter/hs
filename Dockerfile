# Fedora 34 has llvm 12 as "llvm"
FROM fedora:34
RUN dnf install -y git xz llvm-devel stack
COPY . /hs
WORKDIR /hs
RUN rm -rf .stack-work
RUN stack build
