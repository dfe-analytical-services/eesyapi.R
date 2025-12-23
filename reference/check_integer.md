# Check if a value is an integer

is.integer checks the object class, not the value, so credit to VitoshKa
on stack overflow for the core of this function...

## Usage

``` r
check_integer(x)
```

## Arguments

- x:

  a value to test

## Value

logical, false if not an integer, true if an integer

## Details

https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer

looks like it's been adopted in installr too, avoiding needing that as a
dependency by putting the code we need here.
