### Writer monad example

This is just a simple example of how to use the *Writer*
monad to carry information alongside comments in a computation.

### Running

```
stack build
stack exec disclaimer
```

### Output

```
(42,[])
(13993,["13 Boss hates 13, 7 is better","5000 can cause Y2K bug. Back to 1999"])
```