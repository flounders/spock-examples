# guest-database

This example has a number of features such as database use and lucid templating.

# Running

As of this writing Spock has a slight problem with version bounds. To get around
this we will build with stack using resolver lts-12.16.

To build:

```
$ stack init --resolver=lts-12.16
$ stack build
```

Then to run it:

```
$ stack exec -- guest-database
```

The server should be running on http://localhost:8080.
