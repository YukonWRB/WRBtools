# WRBtools

Utilities used across the Water Resources Branch disciplines.

------------------------------------------------------------------------

This package should contain *only* utilities used by or useful to more than one discipline within the WRB. For example, the ability to fetch data from the Aquarius web-hosted server is of use to hydrology (via the WRBfloods package) and to groundwater (via the WRBGW package).

It is imperative that you consider all possible use-cases when designing or modifying functions within this package, as they are or will be dependencies to multiple packages with different lead authors. Changes to function outputs can be especially problematic. Consequently, this project shall be governed according to the following rules:

1.  Function development should **begin** with a tightly defined set of desired outputs. Please consider all possible use-cases.

2.  Functions should be quick to execute as they are intended to be used often. This may involve creating local data repositories, using the data.table package for table operations, reducing calls to external packages, and code efficiency in general.

3.  Dependencies on external packages should be kept to a minimum, reducing the possibilities for code breaks.

4.  Use the minimum number of options in functions while maintaining all core functionality. Example: real-time WSC data occasionally has anomalies which can be filtered out; this is useful for all applications and is a core functionality; at the same time it is sometimes useful to see the unaltered data, so the option to filter is part of the function's core options.

5.  Function outputs should be clearly named. This applies to list elements, data.frames, and even data.frame column headers.

6.  Functions **MUST** be clearly and thoroughly documented.

7.  A package vignette shall be developed and maintained by the authors.

------------------------------------------------------------------------

To ensure that function outputs remain consistent through time and after input by various authors, the following rules shall also apply:

1.  Each function **must** be accompanied by a test file.

2.  Each test file **must** thoroughly test the related function to ensure that package outputs remain consistent and usable by all other packages.

3.  Strongly consider building tests at the outset, or at least sketching out what you plan to test. This should go hand in hand with defining the desired function outputs.
