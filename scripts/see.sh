#!/bin/sh

perl -pe '
  @res = ();
  foreach $p (split /;/) {
    if ($p > 0 && $p < 1000) {
      push @res, sprintf("%.1e", $p);
    } elsif ($p >= 1000) {
      push @res, sprintf("%.0fMB", $p >> 20);
    } else {
      push @res, $p;
    }
  }
  $_ = join ";", @res;
' | column -nts\; | less -S
