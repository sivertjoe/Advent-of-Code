FROM greyltc/archlinux-aur

RUN pacman -Syu --noconfirm

# Install make + gcc/g++
RUN pacman -Sy --noconfirm base-devel

# Install node for javascript
RUN pacman -S --noconfirm nodejs
#
# Install golang
RUN pacman -S --noconfirm go

# Already have perl installed

# Ocaml
RUN pacman -S --noconfirm ocaml

# Haskell
RUN pacman -S --noconfirm ghc

# Clojure
RUN pacman -S --noconfirm clojure

#Common lisp
RUN pacman -S --noconfirm sbcl

# Odin
RUN pacman -S --noconfirm llvm clang git
WORKDIR /Odin-install
RUN git clone https://github.com/odin-lang/Odin.git /Odin-install \
    && git checkout de8f6709f7a6d90d89a18b4ba2f177070397b684 \
    && make \
    && ln -s /Odin-install/odin /usr/local/bin/odin

WORKDIR /

# Zig
RUN pacman -S --noconfirm zig

# Julia
RUN pacman -S --noconfirm julia

# Lua
RUN pacman -S --noconfirm lua

# Nim
RUN pacman -S --noconfirm nim

# Swift
RUN aur-install swift-bin


CMD cd /out && make
