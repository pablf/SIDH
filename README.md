# SIDH

SIDH stands for Supersingular isogeny Diffie-Hellman. 

The SIDH library implements cryptographic primitives for the SIDH key exchange
as well as Elliptic Curves and Finite Fields.

## Usage

The library is made of modules:
 - EC
 - Field
 - Keys
 - NatList
 - Protocol

The module Protocol implements a general cryptographic protocol that can cypher and decypher. It can be used
to define other protocols.

Keys module is contains the SIDH key exchange and Protocol has definitions to handle SIDH easily as seen in
'''Example.hs'''.

Field and EC modules are independent of the others and implement Finite Fields and Elliptic Curves, respectively.
It is possible to use the Field module to implement elliptic curves over
fields other than finite.

## Security

Note that SIDH protocol was proven unsafe in 2022. It should not be used in production!