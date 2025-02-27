# Cryptography Specification

## Overview

The consensus specification describes the cryptographic operations used for
hashing, signing and verifying throughout the Diem Payment Network, as well as
on transactions submitted to this network.

We do not specify any of one of these algorithms from first principles, but
leverage and refer to existing specifications, while laying out exhastively
where our implementation choices differ from the existant standards. Our
implementation choices as described are expected to undergo future improvement
through [Diem Improvement Processes](https://github.com/diem/dip) (DIPs).

## Algorithms

### Signature and verification

Diem uses EdDSA with Curve25519 as its workhorse algorithm, both internally
for the consensus and for signing transactions.

#### Algorithmic choices

The Ed25519 algorithm is standardized by the Internet Engineering Task Force (IETF) in
[RFC-8032](https://tools.ietf.org/html/rfc8032) and is about to be, in [FIPS
186-5](https://csrc.nist.gov/publications/detail/fips/186/5/draft). The
corresponding OID is [1.3.101.112](http://oid-info.com/get/1.3.101.112).

Diem's implementation of EdDSA makes choices and slight variations from these
standards in ways that we make precise below:

* Diem uses the PureEdDSA variant of Ed25519 described in RFC 8032. Indeed,
  both RFC 8032 and FIPS 186-5 also specify a pre-hashing mode for their
  variants, whereby the message `m` taken as input to the signature algorithm
  is replaced by a pre-hashed image `PH(m)`, with `PH` a customizable hash
  function. We do not use this legacy interface.
* Both on signature verification and deserialization, we do perform a bounds
  check on the second half of each signature, as required in RFC 8032 §3.4 and
  FIPS186-5 §7.7-1. That is, if a signature is `R∥S`, with `R` and `S` 32-byte
  bytestrings, and `∥` the concatenation, we reject signatures for which the
  scalar `S` is not within the interval [0 ... L-1], with L the order of the
  prime order group generated by the curve's base point.<sup>[1](#f1)</sup> The
  original EdDSA paper<sup>[2](#f2)</sup> and older Ed25519 libraries do not
  always perform this check, which is necessary to avoid malleability.
* When deserializing points —including the point representing a public key, or
  the point forming the first half of a signature (R above)— we reject points
  contained in the small subgroup of the field. A low-order public key breaks
  non-repudiation, and a low-order R leaks private key information. Many
  Ed25519 libraries, though not all, perform this check.
* When verifying a signature, we use a cofactor-less verification, i.e. the
  group equation that a valid signature must pass is `[s]B = R + [k]A`, rather
  than `[8][s]B = [8]R + [8][k]A`, where `s`, `B`, `R`, `k`, `A` are defined as
  in RFC 8032 §5.1.7. We discuss this choice in [the next section](#On
  cofactored vs. cofactor-less verification), and may revisit it through a DIP
  in the future. Ed25519 implementations overwhelmingly employ cofactor-less
  verification.

#### On cofactored vs. cofactor-less verification

Diem a uses cofactor-less verification equation. We note that cofactor-less
verification is stricter than cofactored verification, and that a signer that
follows either RFC 8032 or FIPS 186-5 will never produce a signature that
passes cofactored verification yet fails cofactorless verification.

Nonetheless, there are good reasons to adopt cofactored verification:

- it is required by FIPS 186-5 and RFC 8032,
- it is required to take advantage of batch verification<sup>[2](#f2)</sup>,
- it is required to take advantage of the faster signature verification of
  Antipa et al.<sup>[3](#f3)</sup> and Pornin<sup>[4](#f4)</sup>.

Unfortunately, off-the-shelf EdDSA implementations overwhelmingly use
cofactor-less verification. This presents a security concern specific to the
blockchain context, as a third-party validator implementation may not be able
to comply with a Diem specification that would mandate cofactored verification
(if unable or unwilling to develop their own Ed25519 library).

If this otherwise honest third-party validator was then to run cofactor-less
verification, it could disagree with the results of execution in a block that
would contain a malicious non-conformant signature during speculative
execution. If such a disagreement was to occur in more than a third of
validators —either because they are byzantine *or* because they did not manage
to implement cofactored verification—, it would be possible to stall consensus
by spamming the transaction mempool with such non-conformant signatures<sup>[5](#f5)</sup>.

As a consequence, we require cofactorless verification, but expect a further
DIP to revisit this decision as soon as the ecosystem of Ed255519 libraries
makes the other choice practical.

### Hashing

Diem uses
[SHA3-256](https://www.nist.gov/publications/sha-3-standard-permutation-based-hash-and-extendable-output-functions?pub_id=919061)
as its go-to algorithm.

This is the sole algorithm used in _internal_ hashes, meaning hashing where the
input and output are for consumption by Diem code and data structures.

Diem also uses other hashing functions when conforming to an external
specification, or calling into external libraries. In particular, we would like
to highlight:

- we re-implement the Noise framework's `Noise_IK_25519_AESGCM_SHA256` key
  exchange scheme, for the purpose of network communications. This entails the
  use of a SHA2-256 hash function in the internal key derivation.
- we call into libraries for Ed25519 signature and verification, and those
  libraries hash the content of a message, internally, using SHA2-512 (as per
  RFC 8032).

These external uses notwithstanding, SHA3-256 is the only hashing function
exposed to the Diem developer.

## Domain separation

In Diem, we are signing large values (transactions, block proposals), which
are therefore hashed as a standard prequel of signing. If the hashes for two distinct
values somehow matched, this could have catastrophic consequences —a malicious
actor could lead you to sign a weird transaction with one expected meaning, which in
fact would produce a signature for something actually different.

How would this happen? We use cryptographically secure hash functions that are
collision-resistant, so it's improbable that two distinct hashing inputs would
produce the same hash. However, when we hash Diem values, we hash their
serialized bytes. A serialization library is in general not collision
resistant, so we have to add extra precautions for this purpose, something
called domain separation.

We describe our [serialization](Serialization) in the next section. For the
moment, suffice to say that for every Diem data structure, we also define
and specify a domain separator.

More specifically, the domain separator [starts with a
prefix](https://github.com/diem/diem/blob/ca14568db9df8c7008dfc6698392ef479e013cf6/crypto/crypto/src/hash.rs#L114-L117)
defined as an ASCII-encoded byte-string:

```rust
"DIEM::"
```

This is concatenated with the
[`trace_name`](https://docs.rs/serde-name/0.1.0/serde_name/index.html) of the
data structure (also ASCII byte-string encoded) to form the domain separator
for this data structure type. This trace name is the serialization name as used
and documented in our [serialization library](Serialization).

Note that while the serialization name coincides with the name of the structure
in Rust for many such types, it is overridable, and we expect such overrides to
occur over time, at least for the purpose of versioning. Our serialization
library includes tooling to [extract the latest serialization
specification](https://github.com/diem/diem/tree/main/testsuite/generate-format/tests/staged),
including the trace name, from code, and developers are advised to look for
changes therein across Diem releases.

We will look at how the domain separator defined in this section is used
[below](#Signing and Verifying).

## Serialization

Serialization of a payload is done using
[BCS](https://docs.rs/bcs/). This
serialization library guarantees deterministic serialization for any Diem data
structure.

This library leverages the ubiquitous serde serialization and deserialization framework
for Rust, and
[serde-reflection](https://github.com/novifinancial/serde-reflection), a
format description and polyglot code generator for serde.

<!-- TODO(fga): refer to serialization spec more fully. -->

## Signing and Verifying

Signing a Diem data structure (including e.g. `RawTransaction` or
`BlockData`), or verifying a signature thereof, involves the following steps:

- producing the byte representation of the message that data structure consists
  of,
- passing the message and the private key to an Ed25519 signing or verification
  implementation that complies with the [choices](Algorithmic choices)
  described above,
- returning the signature or verification result produced by that algorithm.

This byte representation is composed of two parts: the hash of the domain
separator, concatenated with the serialized bytes of the data structure. The
domain separator and hash are as defined above.

That is, if `v` is the Diem value we aim to sign, the bytes sent to the
signing or verification algorithm are:

`SHA3-256( domain_separator(type_of(v)) ) ∥ bcs_bytes(v)`

where ∥ is byte sequence concatenation. The purpose of using the hashed domain
separator, rather than the separator itself is to avoid any suffix attacks on
serialization bytes that would rely on the ambiguity on the byte offset that
marks the end of the domain separator and the beginning of the structure. With
our construction, the (hashed) domain separation information comprises the
first 32 bytes of the binary representation exactly.

### In Rust

In the Diem code base, any structure that implements the derivable
`CryptoHash` trait and `Serialize` can be passed in message position to the
`sign` function implemented on private keys, and to the `verify` function
implemented on public keys and signatures. The construction of the binary
representation is performed implicitly.

When defining a new data structure in the Diem code base, the developer should
implement `Serialize`, and derive `CryptoHasher` and `BCSCryptoHash` by
annotation. The `CryptoHash` trait will be derived automatically.

## Multi-signatures

Diem also supports a transaction signature format which implements
multi-signatures, with signer accountability. This type of signature supports K
out-of N semantics, for K ≤ N ≤ 32.

The implementation, exposed as a Move standard library module
[(Authenticator)](https://github.com/diem/diem/blob/main/language/diem-framework/core/sources/Authenticator.move),
is currently simply a multiple-signature container —rather than a distinct
signing scheme (e.g. MuSig). Nonetheless, we plan to improve it in further
DIPs, while expanding the scope of threshold signing.

We refer the reader to the Move standard library specification for the details
of smart-contract semantics.

<!-- TODO(fga): refer to the Move stdlib doc more fully -->

The input to the multi-public key smart contract consists of a `MultiEd25519PublicKey`,
which contains:

- a vector of N public keys (each present irrespective of whether they
  participated in signing), which elements are 32-bit Ed25519 public keys as
  defined above,
- and a threshold K, providing the minimum number of individual signatures
  which must verify for a multi-signature to be considered valid against this
  public key.

The multi-signature itself, named `MultiEd25519Signature`, consists of two components:

- a vector of N signatures keys (each present irrespective of whether they
  consist of a *valid* signatures), which elements are 64-bit Ed25519
  signatures as defined above,
- a 4-byte (32 bits) bit-vector of length N, where a bit being set in position
  p in the vector indicates a claim that the public key at that position in the
  corresponding `MultiEd25519PublicKey` participated in signing.

The verification of such a signature proceeds as follows:

- we check that the key vector of the `MultiEd25519PublicKey` and the signature
  vector of `MultiEd25519Signature` have the same length, and reject the
  signature otherwise,
- we compare the weight (number of set bits) of the bit vector and the
  threshold set in the public key, and reject the signature if the bit weight
  is strictly lower,
- we check the individual signature-public key pairs located at indexes of set
  bits of the bit vector, and reject the signature if any such individual
  signature verification fails.
- otherwise we accept the signature.

This scheme aims at enforcing the following principles:

- accepted multi-signatures will be recorded on chain, and should only contain
  cryptographically valid content,
- the checking of a bitmap's entire claim —even if it contains more signatures
  than the threshold— gives the parent smart contract the option of limiting
  the malleability of its signatures, by e.g. accepting only precisely K
  signatures.

---

<a name="f1">1</a>: [_The Provable Security of Ed25519: Theory and Practice_, by
    Brendel et al.](https://eprint.iacr.org/2020/823)

<a name="f2">2</a>: [_High-speed, high-security signatures_, Bernstein et al.](https://ed25519.cr.yp.to/ed25519-20110705.pdf)

<a name="f3">3</a>: [_Accelerated Verification of ECDSA Signatures_, Antipa et al.](https://doi.org/10.1007/11693383_21)

<a name="f4">4</a>: [_Optimized Lattice Basis Reduction In Dimension 2, and Fast Schnorr and
    EdDSA Signature Verification_, Pornin](https://eprint.iacr.org/2020/454)

<a name="f5">5</a>: More details in the [consensus specification](../consensus/spec.md).
