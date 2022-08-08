The /inst folder holds files which are installed to the top directory upon package installation. This is a useful place to store templates and sometimes data.

A secondary use (though probably not a proper use!) is to store scripts which are *not* intended to be part of the final build without R CMD CHECK or tests looking for/at them. The catch with that is that these improper files MUST be deleted after you're done with them!
