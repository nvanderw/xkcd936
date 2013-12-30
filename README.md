Building
========

The best way to build with a recent version of Cabal is using a sandbox:

    cabal sandbox init

This package depends on the Haskell "salt" package, which is not currently
available on Hackage. As such, you'll need to [download
it](https://github.com/thoughtpolice/salt), and then run:

    cabal sandbox add-source salt/

to make it available within the sandbox. Next, run:

    cabal install --only-dependencies
    cabal build

Using
=====

To list command-line options, run:

    xkcd936 -h

A typical invocation will look like:

    xkcd936 -d /usr/share/dict/words -e 80 -v -n1

This generates a single password (-n1) using the word list at
/usr/share/dict/words. The -e flag specifies the minimum entropy of
a generated password, and is used to compute the password length in
words. As such, -e and -l are mutually exclusive. -e is recommended
over -l because it allows you to make guarantees about your password
strength!

This utility requires a dictionary file of your choosing, which is simply
a list of possible words separated by newlines. This dictionary can be
specified using the -d option, but will default to stdin. You can take
advantage of this by piping whatever word list you'd like into the generator.
For instance, you can generate passwords consisting of just random characters
by doing something like:

    echo -n 'abcdefghijklmnopqrstuvwxyz' | sed 's/\(.\)/\1\n/g' | xkcd936 -v -l25 -n5 -s ''

The output of that command looks like:

    Number of words: 26
    Password entropy: 117.51099295352732 bits
    hfkbudkdvqvncnnsllupvkpod
    jdyldxkgkfbhqggtlpmpzhart
    ceahhxrybmqaemzvivgethdqt
    pfwdhijkmavqgvmlyvkycwxqw
    oqbmtftnsdgmepfmhjaowxkse

Where the first two lines (the entropy information) are printed to stderr,
and the rest is written to stdout.
