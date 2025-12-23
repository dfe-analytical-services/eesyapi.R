# Controllable console messages

Quick expansion to the
[`message()`](https://rdrr.io/r/base/message.html) function aimed for
use in functions for an easy addition of a global verbose TRUE / FALSE
argument to toggle the messages on or off

## Usage

``` r
toggle_message(..., verbose)
```

## Arguments

- ...:

  any message you would normally pass into
  [`message()`](https://rdrr.io/r/base/message.html). See
  [`message`](https://rdrr.io/r/base/message.html) for more details

- verbose:

  logical, usually a variable passed from the function you are using
  this within
