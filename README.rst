Solving the Matasano Crypto Chanllenges with Haskell
====================================================

I'm using the Matasano Crypto Challenges as an excuse to learn Haskell.

This file gives an overview of the solutions to each problem that I've
solved so far.


Running the programs
--------------------
I have used the latest stable version of the `Haskell Platform`_ under
Linux.

To compile the programs, install the Haskell Platform, unpack the source
tarball, and run the following commands on the top-level
source directory::

  $ cabal install --only-dependencies
  $ cabal configure
  $ cabal build

Executables will be written to the directories ``dist/build/problem-*``.


Problem 1: Convert hex to base64 and back
-----------------------------------------

The program ``problems/problem-1.hs`` converts the given hex
string representation to the Haskell type ``Data.ByteString``, which I
will be using throughout all other problems, and then it converts the
byte string to its Base64 representation. If the resulting value matches
the given string, it will print ``OK``.

The conversion functions are implemented in the module ``Matasano``,
in ``src/Matasano.hs``.


Problem 2: Fixed XOR
--------------------
The program ``problems/problem-2.hs`` XOR's the given hex-encoded byte
strings with the given, verifies that it matches the expected value, and
prints its ASCII representation (``the kid don't play``).

It uses the function ``Matasano.xorEncrypt``, which will be used in the
following problems.


Problem 3: Single-character XOR Cipher
--------------------------------------
The program ``problems/problem-3.hs`` guesses the single-byte XOR key
used to encrypt the given ciphertext, and prints the original plaintext
(``Cooking MC's like a pound of bacon``).

It compares the character frequencies of all possible keys with the
character frequencies of an English language corpus (the adventures of
Sherlock Holmes, from Project Gutenberg, bundled here in
``data/pg1661.txt``). The interesting code is mainly in the functions
``corpusFrequencies``, ``frequencies`` and ``rank`` in the ``Matasano``
module.


Problem 4: Detect single-character XOR
--------------------------------------
The program ``problems/problem-4.hs`` finds the single-byte XOR key
used to encrypt the given message, and prints the plaintext
(``Now that the party is jumping\n``)

The Github gist file is also bundled here in ``data/gist-3132713``.


Problem 5: Repeating-key XOR Cipher
-----------------------------------
The program ``problems/problem-5.hs`` encrypts the given plaintext
with the given repeating XOR key, and checks that the output is the
expected value


Problem 6: Break repeating-key XOR
----------------------------------
The program ``problems/problem-6.hs`` finds the XOR key
(``Terminator X: Bring the noise``) used to encrypt the plaintext
(``I'm back and I'm ringin' the bell[..]``) and prints both.

The Github gist file is also bundled here in ``data/gist-3132752``.


Problem 7: AES in ECB Mode
--------------------------
The program ``problems/problem-7.hs`` uses some Haskell library to
decrypt the given ciphertext (also bundled in ``data/gist-3132853``)
with the given key, and prints the plaintext
(``I'm back and I'm ringin' the bell[..]``)


Problem 8: Detecting ECB
------------------------
The program ``problems/problem-8.hs`` scans all hex-encoded
ciphertexts from ``data/gist-3132928``, and prints the only one which
contains repeated 16-byte chunks.

The line in question starts with ``d880619740a8a19b7840a8a31c810a[..]``,
and contains the following 4-byte blocks repeated 4 times:

  * ``\bd\154\247``
  * ``\r\192oO``
  * ``tL\210\131``
  * ``\213\210\214\156``


Problem 9: Implement PKCS#7 padding
-----------------------------------
The program ``problems/problem-9.hs`` verifies that the byte string
``"YELLOW SUBMARINE"``, padded to a chunk size of 20 with the function
``Matasano.pkcs7Pad``, equals the expected value.


Problem 10: Implement CBC Mode
------------------------------
The program ``problems/problem-10`` decrypts the ciphertext from
``data/gist-3132976`` using the function ``M.decryptAES_CBC``, and
verifies the decryption by encrypting it with ``N.encryptAES_CBC`` and
comparing the resulting cyphertext to the original input.

The first lines of the decrypted text read:

    I'm back and I'm ringin' the bell
    A rockin' on the mike while the fly girls yell
    In ecstasy in the back of me
    Well that's my DJ Deshay cuttin' all them Z's
    Hittin' hard and the girlies goin' crazy
    Vanilla's on the mike, man I'm not lazy.


  .. _`Haskell Platform`: http://www.haskell.org/platform/
