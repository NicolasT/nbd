nbd
===
'nbd_' is a Haskell library to implement NBD__ (Network Block Device) servers.
This can be used to implement block-devices in userspace using the Haskell_
programming language, using the Conduit_ library.

.. _nbd: https://github.com/NicolasT/nbd
.. __: http://en.wikipedia.org/wiki/Network_block_device
.. _Haskell: http://www.haskell.org
.. _Conduit: http://hackage.haskell.org/package/conduit

Demo
----
A very simple demonstration server is included in this repository, see
`bin/server.hs`_. The server can export multiple files (all as a different
device), and currently only supports read-only access.

To test, build the library and binary using cabal_, then run something like::

    $ dd if=/dev/urandom of=blocks bs=1024 count=65536
    $ md5sum blocks 
    7604d00d72b9b8f8cb10e709a1ac3075  blocks
    $ ./dist/build/nbd-demo-server/nbd-demo-server blocks

Now in another shell, as root::

    # modprobe nbd
    # # Connect to the server, creating /dev/nbd0
    # nbd-client localhost /dev/nbd0 -N blocks
    Negotiation: ..size = 64MB
    bs=1024, sz=67108864 bytes
    # # Read the data and validate the hash
    # dd if=/dev/nbd0 bs=4096 | md5sum
    16384+0 records in
    16384+0 records out
    67108864 bytes (67 MB) copied, 0.265507 s, 253 MB/s
    7604d00d72b9b8f8cb10e709a1ac3075  -
    # # Disconnect /dev/nbd0
    # nbd-client -d /dev/nbd0
    Disconnecting: que, disconnect, sock, done

.. _bin/server.hs: blob/master/bin/server.hs
.. _cabal: http://www.haskell.org/cabal/
