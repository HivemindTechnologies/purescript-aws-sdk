let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.2-20210613/packages.dhall sha256:64d7b5a1921e8458589add8a1499a1c82168e726a87fc4f958b3f8760cca2efe

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
